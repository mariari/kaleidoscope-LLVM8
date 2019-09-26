module Emit where



import Protolude hiding (Type, moduleName, local, zero, one)
import Prelude          (error)
import Data.String

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import           Data.ByteString.Short
import qualified Data.Map as Map

import           Codegen
import           JIT
import qualified Syntax as S


--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

one :: AST.Operand
one = cons $ C.Float (F.Double 1.0)

zero :: AST.Operand
zero = cons $ C.Float (F.Double 1.0)

false :: AST.Operand
false = zero

true :: AST.Operand
true = one

toSig :: [ShortByteString] -> [(AST.Type, AST.Name)]
toSig = fmap (\x -> (double, AST.Name x))

codegenTop :: S.Expr -> SymbolTable -> LLVM SymbolTable
codegenTop (S.Function name args body) tbl = do
  op <- define double name fnargs bls
  return (Map.insert name op tbl)
  where
    argsByte = fromString <$> args
    fnargs   = toSig argsByte
    bls = createBlocks
        $ execCodegen tbl
        $ do
          entry <- addBlock entryBlockName
          _     <- setBlock entry
          traverse_ (\a -> do
                        var <- alloca double
                        _   <- store var (local (AST.mkName a))
                        assign a var
                    ) args
          -- add type to map for recursive definitions
          makeFn double fnargs name
          cgen body >>= ret

-- TODO :: figure out why this generated code doesn't show up!
codegenTop (S.Extern name args) tbl = do
  op <- external double name fargs
  return (Map.insert name op tbl)
    where
      fargs = toSig (fromString <$> args)
codegenTop (S.BinaryDef name args body) tbl =
  codegenTop (S.Function ("binary" ++ name) args body) tbl

codegenTop (S.UnaryDef name args body) tbl =
  codegenTop (S.Function ("unary" ++ name) args body) tbl

codegenTop exp tbl = do
  op <- define double "main" [] blks
  return (Map.insert "main" op tbl)
  where
    blks = createBlocks
         $ execCodegen tbl
         $ do
           entry <- addBlock entryBlockName
           _     <- setBlock entry
           cgen exp >>= ret

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.Float n)      = return $ cons $ C.Float (F.Double n)
cgen (S.Var x)        = getvar x >>= load
cgen (S.Call fn args) = do
  fnargs <- traverse cgen args
  fn     <- externf (AST.mkName fn)
  call fn fnargs
cgen (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  -- %entry
  ------------------
  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test ifthen ifelse -- Branch based on the condition

  -- if.then
  ------------------
  setBlock ifthen
  trval <- cgen tr       -- Generate code for the true branch
  br ifexit              -- Branch to the merge block
  ifthen <- getBlock

  -- if.else
  ------------------
  setBlock ifelse
  flval <- cgen fl       -- Generate code for the false branch
  br ifexit              -- Branch to the merge block
  ifelse <- getBlock

  -- if.exit
  ------------------
  setBlock ifexit
  phi double [(trval, ifthen), (flval, ifelse)]
cgen (S.For ivar start cond step body) = do
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"

  -- %entry
  ------------------
  i <- alloca double
  istart <- cgen start           -- Generate loop variable initial value
  stepval <- cgen step           -- Generate loop variable step

  store i istart                 -- Store the loop variable initial value
  assign ivar i                  -- Assign loop variable to the variable name
  br forloop                     -- Branch to the loop body block

  -- for.loop
  ------------------
  setBlock forloop
  cgen body                      -- Generate the loop body
  ival <- load i                 -- Load the current loop iteration
  inext <- fadd ival stepval     -- Increment loop variable
  store i inext

  cond <- cgen cond              -- Generate the loop condition
  test <- fcmp FP.ONE false cond -- Test if the loop condition is True ( 1.0 )
  cbr test forloop forexit       -- Generate the loop condition

  -- for.exit
  ------------------
  setBlock forexit
  return zero
cgen (S.UnaryOp op a) =
  cgen $ S.Call ("unary" ++ op) [a]
cgen (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> cgen (S.Call ("binary" ++ op) [a,b])


lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test


binops :: (Ord k, IsString k)
       => Map k (AST.Operand -> AST.Operand -> Codegen AST.Operand)
binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
    ]

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

codegen :: AST.Module -> [S.Expr] -> SymbolTable -> IO (AST.Module, SymbolTable)
codegen mod fns tbl = do
  modul <- runJIT newast
  return (modul, tbl')
  where
    modn           = foldM (flip codegenTop) tbl fns
    (tbl', newast) = runLLVM mod modn


initModule :: AST.Module
initModule = emptyModule "my cool jit"

test :: IO (AST.Module, SymbolTable)
test = codegen initModule [ (S.Function "b" ["a"] (S.Var "a"))
                          , S.Call "b" [S.Float 2]
                          ]
               Map.empty

tesa :: IO (AST.Module, SymbolTable)
tesa = codegen initModule [ S.Extern "sin" ["x"]
                          ]
               Map.empty
