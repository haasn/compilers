{-# LANGUAGE TemplateHaskell #-}
module Core.FTIM where

import Core.TIM.Flatten

import Control.Lens hiding (perform)
import Control.Monad.State
import Data.Array
import Data.Array.Lens
import Data.Map (Map)
import qualified Data.Map as Map

-- These are deliberately all integers, the type synonyms are for sanity
-- Note the overlap between Addr namespaces: for code and for heap addresses

type Addr    = Int
type Code    = Addr
type Tag     = Int
type Closure = (Code, Addr)
type Frame   = Array Int Closure

data FTIM = FTIM
  { _stack :: [Addr]
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

eval :: State FTIM ()
eval = forever $ getInstruction >>= perform

perform :: FInstr Code -> State FTIM ()
perform = undefined

-- Helpers

getInstruction :: State FTIM (FInstr Code)
getInstruction = do
  addr <- use cptr
  cptr += 1
  use (code.ix addr)
