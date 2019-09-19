module Emit where


import LLVM.Module
import LLVM.Context

import Protolude hiding (Type, moduleName, local)
import Prelude          (error, String, fail)

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import           Data.String.Transform
import           Data.ByteString.Short
import qualified Data.Map as Map

import           Codegen
import qualified Syntax as S



toSig :: [ShortByteString] -> [(AST.Type, AST.Name)]
toSig = fmap (\x -> (double, AST.Name x))

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) =
  define double name fnargs bls
  where
    argsByte = toShortByteString <$> args
    fnargs   = toSig argsByte
    bls = createBlocks
        $ execCodegen
        $ do
          entry <- addBlock entryBlockName
          _     <- setBlock entry
          traverse_ (\a -> do
                        var <- alloca double
                        _   <- store var (local (AST.mkName a))
                        assign a var
                    ) args
          cgen body >>= ret

codegenTop (S.Extern name args) = external double name fargs
    where
      fargs = toSig (toShortByteString <$> args)

codegenTop exp = define double "main" [] blks
  where
    blks = createBlocks
         $ execCodegen
         $ do
           entry <- addBlock entryBlockName
           _     <- setBlock entry
           cgen exp >>= ret

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.Float n)      = return $ cons $ C.Float (F.Double n)
cgen (S.Var x)        = getvar x >>= load
cgen (S.Call fn args) = traverse cgen args >>= call (externf (AST.mkName fn))
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

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext
                $ \context ->
                    withModuleFromAST context newast
                    $ \m -> do
                      llstr <- moduleLLVMAssembly m
                      putStrLn llstr
                      return newast
  where
    modn = traverse codegenTop fns
    newast = runLLVM mod modn
