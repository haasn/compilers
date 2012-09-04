{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Core.FTIM where

-- Simulate a simple state machine with two stacks, three registers
-- and a heap, this has no real use other than testing that the code
-- generators work correctly.

import Core.Types (Tag)
import Core.TIM.Flatten

import Control.Applicative
import Control.Lens hiding (perform)
import Control.Monad.State
import Data.Array
import Data.Array.Lens
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

import System.IO.Unsafe (unsafePerformIO)

-- These are deliberately all integers, the type synonyms are for sanity
-- Note the overlap between Addr namespaces: for code and for heap addresses

type Addr    = Int
type Code    = Addr
type Closure = (Code, Addr)
type Frame   = Array Int Closure

data FTIM = FTIM
  { _stack :: [Closure]
  , _tags  :: [Tag]
  , _heap  :: Map Addr Frame

  -- Three registers: frame pointer, data pointer and code pointer
  , _fptr  :: Addr
  , _dptr  :: Addr
  , _cptr  :: Addr

  -- Read-only code block
  , _code  :: Array Code (FInstr Code)
  }
  deriving Show

makeLenses ''FTIM

-- Primitive evaluation cycle

type Eval = State FTIM ()

-- eval :: Array Code (FInstr Code) -> [Tag]
-- eval code = view tags $ execState step initial

initial :: Array Code (FInstr Code) -> FTIM
initial code = FTIM
  { _stack = []
  , _tags  = []
  , _heap  = Map.empty
  , _fptr  = 0
  , _dptr  = 0
  , _cptr  = 0
  , _code  = code
  }

step :: Eval
step = getInstruction >>= perform

perform :: FInstr Code -> Eval
perform i = case i of
  FTake n m  -> runTake n m
  FLoad c    -> runLoad c
  FPush i    -> runPush i
  FStore n a -> runStore n a
  FCopy  n m -> runCopy n m
  FEnter n   -> runEnter n
  FJump a    -> runJump a
  FSwitch as -> runSwitch as
  FRet t     -> runRet t

-- Imperative-looking primitives, to simulate what a real machine could do

runTake :: Int -> Int -> Eval
runTake n m = do
  cs <- pop n stack
  a  <- alloc m cs
  fptr .= a

runLoad :: Addr -> Eval
runLoad c = do
  a <- use fptr
  push stack (c,a)

runPush :: Int -> Eval
runPush n = do
  f <- use fptr >>= deref
  push stack (f!n)

runStore :: Int -> Addr -> Eval
runStore n c = do
  a <- use fptr
  heap.traverseAt a.ix n .= (c,a)

runCopy :: Int -> Int -> Eval
runCopy n m = do
  a <- use fptr
  d <- use dptr >>= deref
  heap.traverseAt a.ix n .= d!n

runEnter :: Int -> Eval
runEnter n = do
  f <- use fptr >>= deref
  let (c,a) = f!n
  fptr .= a
  cptr .= c

runJump :: Addr -> Eval
runJump a = cptr .= a

runSwitch :: [(Tag, Addr)] -> Eval
runSwitch jt = do
  [t] <- pop 1 tags
  go t jt
  where
    -- Deliberately primitive loop
    go _ [] = error "End of switch"
    go t ((p,a):jt)
      | t == p    = cptr .= a
      | otherwise = go t jt

runRet :: Tag -> Eval
runRet t = do
  f <- use fptr
  dptr .= f
  push tags t

  -- Check for empty stack
  empty <- use (stack.to null)
  if empty
    then error "End of execution"
    else do
      [(c,a)] <- pop 1 stack
      fptr .= a
      cptr .= c

-- Helpers

getInstruction :: State FTIM (FInstr Code)
getInstruction = do
  addr <- use cptr
  cptr += 1
  use (code.ix addr)

push :: Simple Lens FTIM [a] -> a -> State FTIM ()
push l a = l %= (a:)

pop :: Int -> Simple Lens FTIM [a] -> State FTIM [a]
pop n l = do
  (a,b) <- splitAt n <$> use l
  l .= b
  return a

deref :: Addr -> State FTIM Frame
deref = fmap fromJust . use . (heap.) . at

alloc :: Int -> [Closure] -> State FTIM Addr
alloc m cs = do
  let n = length cs
      f = listArray (0, n+m-1) (cs ++ replicate m (0,0))
  -- Generate a fresh address
  addr <- maybe 0 succ <$> use (heap.to Map.maxViewWithKey.to (fmap (fst.fst)))
  heap %= Map.insert addr f
  return addr
