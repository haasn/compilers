{-# LANGUAGE RecordWildCards #-}
module STG.SDCC where

-- SDCC code generator

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
  prelude
  -- Assign the global addresses and initialize the heap pointer
  zipWithM_ (\s n -> put ("const short _" ++ lhs s ++ " = " ++ show n ++ ";"))
    bindings [0..]
  put ("short heapptr = " ++ show (length bindings) ++ ";")
  br
  mapM_ putBinding bindings
  br
  put "void main (void) {"
  indent $ do
    put "short x = 0;" -- Much more efficient code generation
    forM_ (map lhs bindings) $ \n ->
      put ("heap[x++] = (short) &c_" ++ n ++ ";")
    br
    put "ENTER (_main); }"

putBinding :: Binding -> Gen ()
putBinding (Binding n LF{..}) = do
  put("void c_" ++ n ++ " (void) __naked {")
  indent $ do
    forM_ args $ \a -> put ("short _" ++ a ++ ";")
    -- Get rid of the return address
    put "SHRINK;"
    forM_ args $ \a -> put ("POP (_" ++ a ++ ");")
    br
    putExpr body
  put "}"
  br

putExpr :: Expr -> Gen ()
putExpr (App n as) = do
  forM_ (reverse as) $ \a -> put ("PUSH (_" ++ a ++ ");")
  put ("ENTER (_" ++ n ++ ");")

putExpr (LetRec bs e) = error "Cannot compile LetRec yet!"

{-
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
-}

prelude :: Gen ()
prelude = do
  -- SP is decremented manually for tail calls
  put "__sfr __at (0x81) SP;"
  -- Type definition for function calls, for casting
  put "typedef void (*func) (void);"
  br
  put "#define PUSH(x)  stack[stackptr++] = x"
  put "#define POP(x)   x = stack[--stackptr]"
  put "#define SHRINK   SP -= 2"
  put "#define ENTER(x) SHRINK; ((func) heap[x]) ()"
  br
  put "__xdata short heap[32768];"
  put "__idata __at (0x80) short stack[64];"
  put "char stackptr = 0;"
  put "char __at (0x80) wreg;"
  br

-- Helpers and minor functions

{-
alloc :: Binding -> Gen ()
alloc (Binding n LF{..}) =
  put ("var _" ++ n ++ " = new Fun (" ++ show (length args) ++ ");")

atom :: Atom -> String
atom (Name n) = '_' : n
atom (Lit  n) = "lit (" ++ show n ++ ")"
-}

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
