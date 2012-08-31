{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module Core.TIM.Flatten where

import Core.Common
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
    initial = FS "" Map.empty

    go n c = do
      label .= n
      code  <- mapM translate . fromJust $ m^.at n
      fragments %= Map.insert n code

-- Register an anonymous code fragment with the current label

anon :: Code -> State (FStore Name) Name
anon c = do
  fresh     <- label <%= (++ "'")
  code      <- mapM translate c
  fragments %= Map.insert fresh code
  return fresh

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

-- Testing

seqToArray :: Seq a -> Array Int a
seqToArray s = listArray (0, Seq.length s - 1) (toList s)

test = seqToArray . assemble

test1 = test (compile example1)
test2 = test (compile example2)
test3 = test (compile example3)
test4 = test (compile example4)
test5 = test (compile example5)
test6 = test (compile example6)
test7 = test (compile example7)
test8 = test (compile example8)
