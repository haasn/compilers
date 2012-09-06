{-# LANGUAGE RecordWildCards #-}
module STG.CSharp where

-- C# code generator

import STG.Types

import Control.Monad (zipWithM_)
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.DList as DL

type Gen = ReaderT Int (Writer (DL.DList Char))

runGen :: Gen () -> String
runGen = DL.toList . execWriter . (`runReaderT` 0)

-- Code generators

putProgram :: Program -> Gen ()
putProgram Program{..} = do
  preamble
  putGlobal bindings
  epilogue

putGlobal :: [Binding] -> Gen ()
putGlobal = mapM_ $ \b -> do
  put ("static Fun _" ++ lhs b ++ " = new Fun (delegate {")
  indent (putLF (lhs b) (rhs b))
  put "});"
  br

putBindings :: [Binding] -> Gen ()
putBindings bs = do
  mapM_ (alloc . lhs) bs
  br
  mapM_ putBinding bs

putBinding :: Binding -> Gen ()
putBinding (Binding n lf) = do
  put ("_" ++ n ++ ".f = delegate {")
  indent (putLF n lf)
  put "};"
  br

putLF :: Name -> LambdaForm -> Gen ()
putLF n LF{..} = do
  when upd (putUpdate n)

  forM_ args $ \a -> put ("var _" ++ a ++ " = stack.Pop ();")
  unless (null args) br

  putExpr body

putExpr :: Expr -> Gen ()
putExpr (App n as) = do
  forM_ (reverse as) $ \a -> put ("stack.Push (_" ++ a ++ ");")
  put ("return _" ++ n ++ ";")

putExpr (LetRec bs e) = putBindings bs >> putExpr e

putExpr (Constr t ns) = do
  put ("ireg = " ++ show t ++ ";")
  unless (null ns) $ do
    put ("vars = new Fun[" ++ show (length ns) ++ "];")
    let putV v n = put ("vars[" ++ show n ++ "] = (_" ++ v ++ ");")
    zipWithM_ putV ns [0..]
  put "return stack.Pop ();"

putExpr (Case e ms) = do
  put "stack.Push (new Fun (delegate {"
  indent $ do
    put "switch (ireg) {"
    indent $ do
      mapM_ putMatch ms

      put "default:"
      indent $ put "throw new CaseException (ireg);"
    put "}"
  put "}));"
  br
  putExpr e

putMatch :: Match -> Gen ()
putMatch Match{..} = do
  put ("case " ++ show matchTag ++ ":")
  indent $ do
    let putV v n = put ("var _" ++ v ++ " = vars[" ++ show n ++"];")
    zipWithM_ putV matchVars [0..]
    put "vars = null;"
    br
    putExpr matchBody
  br

putUpdate :: Name -> Gen ()
putUpdate n = do
  put "stack.Push (new Fun (delegate {"
  indent $ do
    put "var myireg = ireg;"
    put "var myvars = vars;"
    put ("_" ++ n ++ ".f = delegate {")
    indent $ do
      put "ireg = myireg;"
      put "vars = myvars;"
      put "return stack.Pop ();"
    put "};"
    put "return stack.Pop ();"
  put "}));"
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
  put "class CaseException : Exception {"
  indent $ do
    put "int t; public CaseException (int i) { t = i; }"
    put "public override string ToString() { return"
    indent $ put "\"Incomplete pattern match for tag = \" + t; }}"
  br
  put "partial class STG {"
  br
  put "static int    ireg = 0;"
  put "static double dreg = 0;"
  br
  put "static Stack<Fun> stack = new Stack<Fun> ();"
  put "static Fun[] vars = null;"
  br

epilogue :: Gen ()
epilogue = do
  put "static void Main () {"
  indent $ do
    put "stack.Push (new Fun (delegate {"
    indent $ do
      put "Console.WriteLine (\"ireg = \" + ireg);"
      put "Console.WriteLine (\"dreg = \" + dreg);"
      put "Environment.Exit (0);"
      put "return null;"
    put "}));"
    br
    put "var next = _main;"
    put "while (true)"
    indent $ put "next = next.f ();"
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
