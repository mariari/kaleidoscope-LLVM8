module Emit where


import LLVM.Module
import LLVM.Context

import Protolude hiding (Type, moduleName, local)

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


cgen :: S.Expr -> Codegen AST.Operand
cgen = undefined
