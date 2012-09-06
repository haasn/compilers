{-# LANGUAGE RecordWildCards #-}
module STG.CSharp where

-- C# code generator

import STG.Types

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.DList as DL
import qualified Data.Map   as Map

type Gen = ReaderT Int (Writer (DL.DList Char))

runGen :: Gen () -> String
runGen = DL.toList . execWriter . (`runReaderT` 0)

-- Code generators

putProgram :: Program -> Gen ()
putProgram Program{..} = do
  preamble
  putBindings bindings
  epilogue

putBindings :: [Binding] -> Gen ()
putBindings bs = do
  mapM_ (alloc . lhs) bs
  br
  mapM_ putBinding bs

putBinding :: Binding -> Gen ()
putBinding (Binding n LF{..}) = do
  put ("_" ++ n ++ ".f = delegate {")

  indent $ do
    when upd (putUpdate n)

    forM_ args $ \n -> put ("var _" ++ n ++ " = args.Pop ();")
    unless (null args) br

    putExpr body

  put "};"
  br

putUpdate :: Name -> Gen ()
putUpdate n = do
  put "cont.Push (new Fun (delegate {"
  indent $ do
    put "var mytag  = tag;"
    put "var myvars = vars;"
    put ("_" ++ n ++ ".f = delegate {")
    indent $ do
      put "tag  = mytag;"
      put "vars = myvars;"
      put "return cont.Pop ();"
    put "};"
    put "return cont.Pop ();"
  put "}));"

putExpr :: Expr -> Gen ()
putExpr (App n as) = do
  forM_ (reverse as) $ \a -> put ("args.Push (_" ++ a ++ ");")
  put ("return _" ++ n ++ ";")

putExpr (LetRec bs e) = putBindings bs >> putExpr e

putExpr (Constr t ns) = do
  put ("tag = " ++ show t ++ ";")
  case length ns of
    0 -> put "vars = null;"
    n -> put ("vars = new Fun[" ++ show n ++ "];")
  let putV v n = put ("vars[" ++ show n ++ "] = (_" ++ v ++ ");")
  sequence_ $ zipWith putV ns [0..]
  put "return cont.Pop ();"

putExpr (Case e ms d) = do
  put "cont.Push (new Fun (delegate {"
  indent $ do
    put "switch (tag) {"
    indent $ do
      mapM_ putMatch ms

      put "default:"
      indent (putExpr d)
    put "}"
  put "}));"
  br
  putExpr e

putMatch :: Match -> Gen ()
putMatch Match{..} = do
  put ("case " ++ show matchTag ++ ":")
  indent $ do
    let putV v n = put ("var _" ++ v ++ " = vars[" ++ show n ++"];")
    sequence_ $ zipWith putV matchVars [0..]
    put "vars = null;"
    br
    putExpr matchBody
  br

preamble :: Gen ()
preamble = do
  put "using System;"
  put "using System.Collections.Generic;"
  br
  put "class Fun {"
  put "  public FunPtr f;"
  put "  public Fun () {}"
  put "  public Fun (FunPtr p) { f = p; }}"
  put "delegate Fun FunPtr ();"
  br
  put "class STG { static void Main() {"
  br
  put "int tag = 0;"
  br
  put "var args = new Stack<Fun> ();"
  put "var cont = new Stack<Fun> ();"
  put "Fun[] vars = null;"
  br

epilogue :: Gen ()
epilogue = do
  put "var output = new Fun ();"
  put "output.f = delegate {"
  put "  Console.WriteLine (tag);"
  put "  Environment.Exit (0);"
  put "  return null;"
  put "};"
  br
  put "cont.Push (output);"
  br
  put "var next = _main;"
  put "while (true)"
  put "  next = next.f ();"
  br
  put "}}"

-- Helpers and minor functions

alloc :: Name -> Gen ()
alloc n = put ("var _" ++ n ++ " = new Fun ();")

indent :: Gen a -> Gen a
indent = local (+2)

put :: String -> Gen ()
put s = do
  i <- ask
  tell $ DL.fromList (replicate i ' ')
  tell $ DL.fromList s
  br

br :: Gen ()
br = tell $ DL.singleton '\n'
