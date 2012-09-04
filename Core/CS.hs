module Core.CS where

-- C# code generator, this re-uses about half of the FTIM generator but sticks
-- with labels instead of code addresses because C# has its own labels/jumps.

import Core.Types
import Core.TIM (store, compile)
import Core.TIM.Flatten

import Control.Lens (view, itraverse)
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.DList as DL
import qualified Data.Map   as Map

type CSW = Writer (DL.DList Char) ()

buildCS :: CoreProgram -> String
buildCS = formatCS . flatten . view store . compile

formatCS :: Map Name (FCode Name) -> String
formatCS m = run $ do
  preamble

  put $ "goto c" ++ show (replace "main") ++ ";"

  itraverse putSC m'

  retCode
  jumpTable (Map.size p - 1)

  put "}}"

  where
    -- A unique number for each identifier
    p         = Map.fromList $ zip (Map.keys m) [0..]
    replace n = fromJust (Map.lookup n p)
    m'        = fmap (fmap $ fmap replace) (Map.mapKeys replace m)
    run       = DL.toList . execWriter

putSC :: Int -> FCode Int -> CSW
putSC n cs = do
  put $ 'c' : show n ++ ":"
  mapM_ putInstr cs

putInstr :: FInstr Int -> CSW
putInstr (FTake n m) = do
  put $ "f=new C[" ++ show (n+m) ++ "];"
  put $ "for(int i=0;i<" ++ show n ++ ";i++)"
  put   "f[i]=s.Pop();"

putInstr (FLoad n) =
  put $ "s.Push(new C(" ++ show n ++ ",f));"

putInstr (FPush i) =
  put $ "s.Push(f[" ++ show i ++ "]);"

putInstr (FStore n a) =
  put $ "f[" ++ show n ++ "]=new C(" ++ show a ++ ",f);"

putInstr (FCopy n m) =
  put $ "f[" ++ show n ++ "]=d[" ++ show m ++ "];"

putInstr (FEnter n) = do
  put $ "j=f[" ++ show n ++ "].c;"
  put $ "f=f[" ++ show n ++ "].f;"
  put   "goto j;"

putInstr (FJump n) =
  put $ "goto c" ++ show n ++ ";"

putInstr (FSwitch as) = do
  put $ "switch(t.Pop()) {"
  forM_ as $ \(t,n) ->
    put $ "case " ++ show t ++ ":goto c" ++ show n ++ ";"
  put "}"

putInstr (FRet t) = do
  put   "d=f;"
  put $ "t.Push(" ++ show t ++ ");"
  put   "goto r;"

preamble :: CSW
preamble = do
  put "using System;"
  put "using System.Collections.Generic;"
  put "class C { public uint c; public C[] f;"
  put "          public C(uint a, C[] b) {c=a;f=b;}}"
  put "class TIM { static void Main() {"
  put "var s=new Stack<C>();"
  put "var t=new Stack<ulong>();"
  put "C[]f=null;"
  put "C[]d=null;"
  put "uint j;"

retCode :: CSW
retCode = do
  put "r:"
  put "if(s.Count==0){"
  put "foreach (var x in t)"
  put "Console.WriteLine(x);"
  put "return;}else{"
  put "j=s.Peek().c;"
  put "f=s.Pop().f;"
  put "goto j;"
  put "}"

jumpTable :: Int -> CSW
jumpTable n = do
  put "j:"
  put "switch(j){"
  forM [0..n] $ \i ->
    put $ "case " ++ show i ++ ":goto c" ++ show i ++ ";"
  put "}"

-- DList helpers

put :: String -> CSW
put = tell . flip DL.snoc '\n' . DL.fromList
