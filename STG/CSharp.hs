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
  preamble isMain
  putGlobal bindings
  epilogue isMain
  where isMain = "main" `elem` map lhs bindings

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


preamble :: Bool -> Gen ()
preamble isMain = do
  put "using System;"
  put "using System.Collections.Generic;"
  br
  when isMain $ do
    put "class Fun {"
    put "  public FunPtr f;"
    put "  public int a = 0;"
    put "  public Fun () {}"
    put "  public Fun (int n) { a = n; }"
    put "  public Fun (FunPtr p) { f = p; }"
    put "  public Fun (int n, FunPtr p) { a = n; f = p; }}"
    put "delegate Fun FunPtr ();"
    br
    put "class CaseException : Exception {"
    put "  int t; public CaseException (int i) { t = i; }"
    put "  public override string ToString() { return"
    put "    \"Incomplete pattern match for tag = \" + t; }}"
    br
  put "partial class STG {"
  br
  when isMain $ do
    put "static int    ireg = 0;"
    put "static double dreg = 0;"
    br
    put "static Stack<Fun> stack = new Stack<Fun> ();"
    put "static Fun[] vars = null;"
    br
    put "static void update (Fun f) {"
    put "  stack.Push (new Fun (1, delegate {"
    put "    var myireg = ireg;"
    put "    var mydreg = dreg;"
    put "    var myvars = vars;"
    put "    f.a = 1;"
    put "    f.f = delegate {"
    put "      ireg = myireg;"
    put "      dreg = mydreg;"
    put "      vars = myvars;"
    put "      return stack.Pop ();"
    put "    };"
    put "    return stack.Pop ();"
    put "}));}"
    br
    put "static Fun lit (int i) {"
    put "  return new Fun (1, delegate {"
    put "    ireg = i;"
    put "    return stack.Pop (); });}"
    br
    putPrimOp Add "+"
    putPrimOp Mul "*"
    putPrimOp Sub "-"
    putPrimOp Div "/"

putPrimOp :: Op -> String -> Gen ()
putPrimOp o s = do
  put("static Fun " ++ show o ++ " (Fun a, Fun b) {")
  put "  return new Fun (delegate {"
  put "    stack.Push (new Fun (delegate {"
  put "      int it = ireg;"
  put "      stack.Push (new Fun (1, delegate {"
  put("        ireg " ++ s ++ "= it;")
  put "        return stack.Pop (); }));"
  put "      return b; }));"
  put "    return a;"
  put "});}"
  br

epilogue :: Bool -> Gen ()
epilogue isMain = do
  when isMain $ do
    put "static void Main () {"
    put "  var handler = new Fun ();"
    put "  handler.f = delegate {"
    put "    switch (ireg) {"
    put "      case 0:"
    put "        throw new Exception (\"Program not terminated properly!\");"
    br
    put "      case 1:"
    put "        var i = vars[0];"
    put "        var n = vars[1];"
    put "        vars = null;"
    br
    put "        stack.Push (new Fun (delegate {"
    put "          Console.Write ((char) ireg);"
    put "          stack.Push (handler);"
    put "          return n;"
    put "        }));"
    br
    put "        return i;"
    br
    put "      case 2:"
    put "        var f = vars[0];"
    put "        vars = null;"
    br
    put "        char key = Console.ReadKey (true).KeyChar;"
    put "        stack.Push (handler);"
    put "        stack.Push (lit (key));"
    br
    put "        return f;"
    br
    put "      case 3:"
    put "        var c = vars[0];"
    put "        vars = null;"
    br
    put "        stack.Push (new Fun (delegate {"
    put "          Environment.Exit (ireg);"
    put "          return null;"
    put "        }));"
    br
    put "        return c;"
    br
    put "      default:"
    put "        throw new CaseException (ireg);"
    put "    }"
    put "  };"
    br
    put "  stack.Push (handler);"
    put "  var next = _main;"
    put "  while (next.a <= stack.Count)"
    put "    next = next.f ();"
    br
    put "  Console.WriteLine (\"weak head normal form\"); }"
  put "}"

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
