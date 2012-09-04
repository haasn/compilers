{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module Core.TIM.Flatten where

-- Compiler for a low level flattened TIM with a sequential code block and
-- simpler instructions, with no special addressing modes.

import Core.Types
import Core.TIM hiding (code)

import Core.Examples

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Array (Array, listArray)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

type FCode a = [FInstr a]
type FAddr   = Int

-- Parametric in address type because we use labels during code generation
-- and then switch to addresses once everything's assembled into place.

data FInstr addr
  -- Build new frame from stack
  = FTake Int Int

  -- Pushing new closures to the stack
  | FLoad addr -- Push code fragment
  | FPush Int  -- Push from current frame

  -- Frame updating instructions
  | FStore Int addr -- Store code fragment
  | FCopy Int Int   -- Copy from data frame

  -- Jumping instructions
  | FEnter Int            -- Enter a closure in the frame
  | FJump addr            -- Jump to a code fragment
  | FSwitch [(Tag, addr)] -- Jump on tag

  -- Push to VStack and pop closure
  | FRet Tag
  deriving (Show, Functor)

-- In the translation process, we update a store to accomodate for local
-- code fragments

data FStore addr = FS
  { _label     :: addr
  , _counter   :: Int
  , _fragments :: Map addr (FCode addr)
  }

makeLenses ''FStore

-- Assemble a TIM down to a flat sequence

assemble :: TIM -> Seq (FInstr FAddr)
assemble tim = FJump (replace m "main") Seq.<| (mapped.mapped %~ replace m) s
  where (m, s) = concatStore . fmap invariant . flatten $ tim^.store

-- Translate to the lower level instruction type

translate :: Instr -> State (FStore Name) (FInstr Name)
translate i = case i of
  Take n m -> return $ FTake n m

  Push (Global n) -> return $ FLoad n
  Push (Code c)   -> FLoad <$> anon c
  Push (Local n)  -> return $ FPush n

  Move i (Global n) -> return    $ FStore i n
  Move i (Code c)   -> FStore i <$> anon c
  Move i (Data d)   -> return    $ FCopy i d

  Enter (Local n)  -> return   $ FEnter n
  Enter (Global n) -> return   $ FJump n
  Enter (Code c)   -> FJump   <$> anon c
  Switch m         -> FSwitch <$> (traverse._2 %%~ anon $ Map.toList m)

  Return t -> return $ FRet t

  _ -> error $ "Cannot translate: " ++ show i

-- Flatten a code store

flatten :: Map Name Code -> Map Name (FCode Name)
flatten m = execState (itraverse go m) initial ^. fragments
  where
    initial = FS "" 0 Map.empty

    go n c = do
      label     .= n
      counter   .= 0
      code      <- mapM translate . fromJust $ m^.at n
      fragments %= Map.insert n code

-- Register an anonymous code fragment with the current label

anon :: Code -> State (FStore Name) Name
anon c = do
  base      <- use label
  fresh     <- counter <+= 1
  let name = base ++ show fresh
  code      <- mapM translate c
  fragments %= Map.insert name code
  return name

-- Assert invariant

invariant :: Show a => FCode a -> FCode a
invariant cs
  | isJump (last cs) = cs
  | otherwise = error $ "Failed invariant: last instruction is not jump: "
                          ++ show cs
  where
    isJump (FEnter  _) = True
    isJump (FJump   _) = True
    isJump (FSwitch _) = True
    isJump (FRet    _) = True
    isJump  _          = False

concatStore :: Ord a => Map a (FCode a) -> (Map a FAddr, Seq (FInstr a))
concatStore = go Map.empty Seq.empty . Map.toList
  where
    go m s [] = (m, s)
    go m s ((n, c):cs) =
      -- The +1 is to make room for the FJump at the start
      go (Map.insert n (Seq.length s + 1) m) (s Seq.>< Seq.fromList c) cs

replace :: Ord a => Map a b -> a -> b
replace m = fromJust . (`Map.lookup` m)
