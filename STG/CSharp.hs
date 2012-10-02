{-# LANGUAGE RecordWildCards #-}
module STG.CSharp where

-- C# code generator

import Prelude
import STG.Types

import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.DList as DL

type Gen = ReaderT Int (Writer (DL.DList Char)) ()

runGen :: Gen -> String
runGen = DL.toList . execWriter . (`runReaderT` 0)

-- Code generators

putProgram :: Program -> Gen
putProgram Program{..} = do
  put "using System;"
  put "using System.Collections.Generic;"
  br
  put "static partial class STG {"
  indent (putGlobal bindings)
  put "}"

putGlobal :: [Binding] -> Gen
putGlobal = mapM_ $ \(n, b) -> do
  put("static Fun _" ++ n ++ " =")
  indent $ putExpr b
  put ";"
  br

putBinding :: Binding -> Gen
putBinding (n, b) = do
  put("_" ++ n ++ ".f = delegate {")
  returns (putExpr b)
  put "};"
  br

putExpr :: Expr -> Gen
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

putExpr (LetRec bs e) = scoped $ do
  forM_ bs $ \(n,_) -> put ("var _" ++ n ++ " = new Fun ();")
  br
  mapM_ putBinding bs
  returns $ putExpr e

putExpr (Prim op a b) = scoped $ do
  put (show op ++ " (")
  indent $ putExpr a
  put ", "
  indent $ putExpr b
  put ")"

putExpr (Literal i) = put $ "lit (" ++ show i ++ ")"

-- Helpers and minor functions

scoped :: Gen -> Gen
scoped a = do
  put "new Fun (delegate {"
  indent a
  put "})"

returns :: Gen -> Gen
returns a = put "return" >> indent a >> put ";"

indent :: Gen -> Gen
indent = local (+2)

put :: String -> Gen
put s = do
  i <- ask
  tell $ DL.fromList (replicate i ' ')
  tell $ DL.fromList s
  br

br :: Gen
br = tell $ DL.singleton '\n'
