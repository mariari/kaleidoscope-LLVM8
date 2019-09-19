module Emit where


import LLVM.Module
import LLVM.Context

import Protolude hiding (Type, moduleName, local)
import Prelude          (error)
import Data.String

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import           Data.ByteString.Short
import qualified Data.Map as Map

import           Codegen
import qualified Syntax as S


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

codegenTop (S.Extern name args) tbl = do
  op <- external double name fargs
  return (Map.insert name op tbl)
    where
      fargs = toSig (fromString <$> args)

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
cgen (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "no such operator"


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
  modul <- withContext
            $ \context ->
                withModuleFromAST context newast
                $ \m -> do
                  llstr <- moduleLLVMAssembly m
                  putStrLn llstr
                  return newast
  return (modul, tbl')
  where
    modn          = foldM (flip codegenTop) tbl fns
    (tbl', newast) = runLLVM mod modn


initModule :: AST.Module
initModule = emptyModule "my cool jit"

test :: IO (AST.Module, SymbolTable)
test = codegen initModule [ (S.Function "b" ["a"] (S.Var "a"))
                          , S.Call "b" [S.Float 2]
                          ]
               Map.empty
