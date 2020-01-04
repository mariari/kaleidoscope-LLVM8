module JIT where

import Protolude
import qualified Data.ByteString.Char8 as ByteString
import Foreign.Ptr ( FunPtr, castFunPtr )


import LLVM.Context
import LLVM.Module as Mod
import qualified LLVM.AST as AST

import LLVM.Target
import LLVM.PassManager

import qualified LLVM.ExecutionEngine as EE

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> (IO Double)

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 3  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module -> IO AST.Module
runJIT mod = do
  withContext $ \context ->
    jit context $ \executionEngine -> do
      initializeAllTargets
      withModuleFromAST context mod $ \m ->
       withPassManager passes $ \pm -> do
          -- Optimization Pass
          _      <- runPassManager pm m
          optmod <- moduleAST m
          s      <- moduleLLVMAssembly m
          ByteString.putStrLn s

          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee "main"
            case mainfn of
              Just fn -> do
                res <- run fn
                putStrLn $ "Evaluated to: " ++ show res
              Nothing -> return ()

          -- Return the NON optimized module
          -- If we optimize code partially it eliminates call of external methods as non used code
          -- So the solution is to accumulate and store non optimized code and optimize it right before evaluation
          return mod
