{-# LANGUAGE RecordWildCards #-}
module STG.CSharp where

-- C# code generator

import Prelude
import STG.Types

import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.DList as DL

type Gen = ReaderT Int (Writer (DL.DList Char))

runGen :: Gen () -> String
runGen = DL.toList . execWriter . (`runReaderT` 0)

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
putGlobal = mapM_ $ \(n, b) -> do
  put("static Fun _" ++ n ++ " =")
  indent $ putExpr b
  put ";"
  br

putBindings :: [Binding] -> Gen ()
putBindings bs = do
  mapM_ alloc bs
  br
  mapM_ putBinding bs

putBinding :: Binding -> Gen ()
putBinding (n, b) = do
  put("_" ++ n ++ ".f = delegate {")
  returns (putExpr b)
  put "};"
  br

putExpr :: Expr -> Gen ()
putExpr (App n []) = put $ '_' : n

putExpr (App n xs) = scoped $ do
  forM_ (reverse xs) $ \x -> do
    put "stack.Push ("
    indent $ putExpr x
    put ");"

  put("return _" ++ n ++ ";")

putExpr (Lambda ns e) = scoped $ do
  forM_ ns $ \a -> put ("var _" ++ a ++ " = stack.Pop ();")
  unless (null ns) br

  returns $ putExpr e

putExpr (LetRec bs e) = scoped $ putBindings bs >> returns (putExpr e)

putExpr (Constr t ns) = scoped $ do
  put ("ireg = " ++ show t ++ ";")
  unless (null ns) $ do
    put ("vars = new Fun[" ++ show (length ns) ++ "];")
    zipWithM_ putV ns [0..]
  put "return stack.Pop ();"
 where
  putV a n = do
    put ("vars[" ++ show n ++ "] =")
    indent (putExpr a)
    put ";"

putExpr (Case e ms) = scoped $ do
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
  returns $ putExpr e

putExpr (Prim op a b) = scoped $ do
  put (show op ++ " (")
  indent $ putExpr a
  put ", "
  indent $ putExpr b
  put ")"

putExpr (Literal i) = put $ "lit (" ++ show i ++ ")"

putMatch :: Match -> Gen ()
putMatch Match{..} = do
  put ("case " ++ show matchTag ++ ":")
  indent $ do
    let putV v n = put ("var _" ++ v ++ " = vars[" ++ show n ++"];")
    zipWithM_ putV matchVars [0..]
    put "vars = null;"
    br
    returns $ putExpr matchBody
  br

-- Helpers and minor functions

alloc :: Binding -> Gen ()
alloc (n, _) = put ("var _" ++ n ++ " = new Fun ();")

scoped :: Gen () -> Gen ()
scoped a = do
  put "new Fun (delegate {"
  indent a
  put "})"

returns :: Gen () -> Gen ()
returns a = put "return" >> indent a >> put ";"

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
