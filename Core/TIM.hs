{-# LANGUAGE TemplateHaskell, RecordWildCards,
             LiberalTypeSynonyms, Rank2Types #-}
module Core.TIM where

-- ‘Three Instruction Machine’, so called because it operates on three
-- dominent instructions: Take, Move and Enter. The second half of this file
-- is an example state machine implementation the TIM instruction set, but
-- this is not needed when only compiling (eg. see FTIM which is based on the
-- output of this compiler).

import Prelude hiding (or, lookup)

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.DList as DL
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map

import Core.Common

data Instr
  = Take Int Int -- Take n items off the stack and store them in a new frame
                 -- The size of the frame is n+m where m is the padding needed
                 -- for letrec definitions

  | Move Int Arg -- Move argument to a given position in the current frame
  | Enter Arg    -- Jump to a given code/frame
  | Push Arg     -- Push a given code/frame to the stack
  -- | Mark Int     -- Mark nth closure in the current frame and dump the stack

  | Return Tag   -- Push a tag on the value stack, update the data frame and
                 -- move into the next continuation

  | Switch (Map Tag Code) -- Jump table, scrutinize the top element of the
                          -- value stack and jump into the appropriate path
  deriving Show

data Arg
  = Local Int    -- A closure in the current frame
  | Data Int     -- A closure in the data frame
  | Global Name  -- A globally named supercombinator
  | Code Code    -- Anonymous code fragment
  deriving Show

-- State machine-based implementation

type Addr    = Integer
type Stack   = [Closure]
type VStack  = [Tag]
type Dump    = [(Stack, Addr, Int)]
type Closure = (Code, Addr)
type Frame   = [Closure]
type Code    = [Instr]
type Heap    = Map Addr Frame
type Store   = Map Name Code

nullPtr :: Addr
nullPtr = 0

data TIM = TIM
  { _stack :: Stack
  , _vals  :: VStack
  , _dump  :: Dump
  , _code  :: Code
  , _frame :: Addr
  , _dataf :: Addr
  , _heap  :: Heap
  , _store :: Store
  }
  deriving Show

makeLenses ''TIM

-- Core compiler

type Env  = Map Name Arg
type Size = Int
type Gen  = ReaderT Env (State Size)

runGen :: Gen a -> Env -> Size -> (a, Size)
runGen g = runState . runReaderT g

compile :: CoreProgram -> TIM
compile (CoreProgram defs) = TIM
  { _stack = []
  , _vals  = []
  , _dump  = []
  , _code  = [Enter (Global "main")]
  , _frame = nullPtr
  , _dataf = nullPtr
  , _heap  = Map.empty
  , _store = Map.fromList $ map (\sc -> (scName sc, mkSC sc initial)) world
  }
  where
    world   = defs ++ prelude
    names   = map scName world
    initial = Map.fromList (zip names (map Global names))

mkSC :: ScDef -> Env -> Code
mkSC ScDef{..} p = case t of
  -- Omit useless Take 0 0 instructions
  0 -> c
  _ -> Take n (t-n) : c
  where
    n      = length scArgs
    (c, t) = runGen (mkR scBody) (augmentD scArgs 0 p) n

mkR :: CoreExpr -> Gen Code
mkR expr = case expr of
  App e1 e2 -> do
    a <- mkA e2
    c <- mkR e1
    return (Push a : c)

  Free _ -> do
    a <- mkA expr
    return [Enter a]

  LetRec bs e -> do
    -- Get the current offset map for future reference and increase it to make
    -- room for the bindings to follow
    d <- get
    put (d + length bs)
    let ns = map fst bs

    -- Augment the local definitions with indirection nodes
    cs <- local (augmentI ns d) (zipWith Move [d..] <$> mapM mkA (map snd bs))

    -- Generate the expression body itself, without indirection
    c  <- local (augmentD ns d) (mkR e)

    return (cs ++ c)

  -- Skip the Take for empty constructors
  Constr t 0 -> return [Return t]
  Constr t a -> return [Take a 0, Return t]

  Case e bs -> do
    sw <- Switch . Map.fromList <$> mapM mkE bs
    cs <- mkR e
    return (Push (Code [sw]) : cs)

mkA :: CoreExpr -> Gen Arg
mkA (Free n) = fromMaybe (error $ "Unknown variable " ++ n) <$> query (at n)
mkA e = Code <$> mkR e

mkE :: Match -> Gen (Tag, Code)
mkE Match{..} = do
  d <- get
  local (augmentD matchVars d) $ do
    let ms = zipWith (\n _ -> Move (d+n) (Data n)) [0..] matchVars
    put (d + length matchVars)
    is <- mkR matchBody

    return (matchTag, ms ++ is)

-- Helpers for code generation

augmentD :: [Name] -> Int -> Env -> Env
augmentD = augment' Local

augmentI :: [Name] -> Int -> Env -> Env
augmentI = augment' (Code . return . Enter . Local)

augment' :: (Int -> Arg) -> [Name] -> Int -> Env -> Env
augment' f names n = Map.union $ Map.fromList (zip names (map f [n..]))

-- TIM state machine

type STIM = State TIM

step :: Instr -> STIM ()
step (Push a)  = lookup a >>= push stack
step (Enter a) = lookup a >>= enter
step (Take n p)  = do
  (cs, rest) <- splitAt n <$> use stack
  addr       <- alloc (cs ++ replicate p ([], nullPtr))

  stack .= rest
  frame .= addr

step (Move n a) = do
  cl   <- lookup a
  addr <- use frame

  heap.at addr %= fmap (move n cl)

step (Return t) = do
  -- Move the current frame pointer into the data frame and push tag
  use frame >>= assign dataf
  push vals t

  empty <- null <$> use stack
  unless empty $ do
    -- Set up the new frame and jump into the closure
    (c,cf) <- pop stack
    code  .= c
    frame .= cf

step (Switch m) = do
  tag <- pop vals
  let c = fromMaybe (error "Non-exhaustive pattern match") (Map.lookup tag m)
  code .= c

{-
step (Mark d) = do
  a <- use frame
  s <- use stack
  push dump (s, a, d)
  -- Clear the stack for further evaluation
  stack .= []
-}

evaluate :: TIM -> TIM
evaluate = execState go
  where
    go = do
      c <- use code
      case c of
        []   -> return () -- Reached WHNF
        i:is -> code .= is >> step i >> go

-- Lookup function for the various addressing modes

lookup :: Arg -> STIM Closure
lookup arg = case arg of
  -- Look up the nth closure in the local/data frame
  Local d  -> view (element d) <$> current
  Data  d  -> view (element d) <$> curdata

  -- Augment the provided code with the current frame pointer
  Global n -> getCode n >>= withCurrent
  Code c   -> withCurrent c
  where
    withCurrent c = (,) c <$> use frame

enter :: Closure -> STIM ()
enter (c, f) = do
  code  .= c
  frame .= f

-- Auxiliary functions

push :: Simple Setting TIM [a] -> a -> STIM ()
push l c = l %= (c:)

pop :: Simple Lens TIM [a] -> STIM a
pop l = do
  s <- use l
  case s of
    []   -> error "Trying to pop on empty stack!"
    a:as -> l .= as >> return a

alloc :: Frame -> STIM Addr
alloc f = do
  -- For this simple implementation, simply get the highest address
  -- and increase it by one, or 0 otherwise.
  addr <- maybe 0 succ <$> use (heap.to Map.maxViewWithKey.to (fmap (fst.fst)))
  heap %= Map.insert addr f
  return addr

move :: Int -> a -> [a] -> [a]
move _ _ [] = error "move: index outside range"
move 0 a (_:xs) = a:xs
move n a (x:xs) = x : move (n-1) a xs

current :: STIM Frame
current = use frame >>= deref

curdata :: STIM Frame
curdata = use dataf >>= deref

deref :: Addr -> STIM Frame
deref a = use (heap.at a) `or` "Failed dereferencing address: " ++ show a

getCode :: Name -> STIM Code
getCode n = use (store.at n) `or` "No such name in code store: " ++ show n

or :: Functor f => f (Maybe a) -> String -> f a
or = flip $ fmap . fromMaybe . error
infixr 1 `or`

assign :: MonadState a m => Setting a a c d -> d -> m ()
assign = (.=)
