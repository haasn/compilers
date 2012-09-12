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

debug :: Bool
debug = False

-- Code generators

putProgram :: Program -> Gen ()
putProgram Program{..} = do
  put "using System;"
  put "using System.Collections.Generic;"
  br
  put "static partial class STG {"
  indent (putGlobal bindings)
  put "}"

putGlobal :: [Binding] -> Gen ()
putGlobal = mapM_ $ \b -> do
  let putA = show . length . args . rhs
  put("static Fun _" ++ lhs b ++ " = new Fun (" ++ putA b ++ ", delegate {")
  indent (putLF (lhs b) (rhs b))
  put "});"
  br

putBindings :: [Binding] -> Gen ()
putBindings bs = do
  mapM_ alloc bs
  br
  mapM_ putBinding bs

putBinding :: Binding -> Gen ()
putBinding (Binding n lf) = do
  put("_" ++ n ++ ".f = delegate {")
  indent (putLF n lf)
  put "};"
  br

putLF :: Name -> LambdaForm -> Gen ()
putLF n LF{..} = do
  when debug $ put ("Console.WriteLine (" ++ show n ++ ");")
  when upd   $ put ("update (_" ++ n ++ ");")

  forM_ args $ \a -> put ("var _" ++ a ++ " = stack.Pop ();")
  unless (null args) br

  putExpr body

putExpr :: Expr -> Gen ()
putExpr (App n as) = do
  forM_ (reverse as) $ \a -> put ("stack.Push (" ++ atom a ++ ");")
  put ("return " ++ atom n ++ ";")

putExpr (LetRec bs e) = putBindings bs >> putExpr e

putExpr (Constr t ns) = do
  put ("ireg = " ++ show t ++ ";")
  unless (null ns) $ do
    put ("vars = new Fun[" ++ show (length ns) ++ "];")
    let putV a n = put ("vars[" ++ show n ++ "] = (" ++ atom a ++ ");")
    zipWithM_ putV ns [0..]
  put "return new Fun (1, delegate { return stack.Pop (); });"

putExpr (Case e ms) = do
  put "stack.Push (new Fun (delegate {"
  indent $ do
    put "switch (ireg) {"
    indent $ do
      mapM_ putMatch ms

      put "default:"
      put "  throw new CaseException (ireg);"
    put "}"
  put "}));"
  br
  putExpr e

putExpr (Prim op a b) =
  put ("return " ++ show op ++ " (" ++ atom a ++ ", " ++ atom b ++ ");")

putMatch :: Match -> Gen ()
putMatch Match{..} = do
  put ("case " ++ show matchTag ++ ":")
  indent $ do
    when debug $ put ("Console.WriteLine (\"case " ++ show matchTag ++ "\");")
    let putV v n = put ("var _" ++ v ++ " = vars[" ++ show n ++"];")
    zipWithM_ putV matchVars [0..]
    put "vars = null;"
    br
    putExpr matchBody
  br

-- Helpers and minor functions

alloc :: Binding -> Gen ()
alloc (Binding n LF{..}) =
  put ("var _" ++ n ++ " = new Fun (" ++ show (length args) ++ ");")

atom :: Atom -> String
atom (Name n) = '_' : n
atom (Lit  n) = "lit (" ++ show n ++ ")"

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
