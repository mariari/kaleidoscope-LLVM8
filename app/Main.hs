{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude
import Prelude (String)

import Parser
import Codegen
import Emit

-- import Control.Monad.Trans
import qualified Data.Map as Map
import System.Console.Haskeline

import qualified LLVM.AST as AST

-- initModule :: AST.Module
-- initModule = emptyModule "my cool jit"

process :: AST.Module -> SymbolTable -> String -> IO (Maybe (AST.Module, SymbolTable))
process modo tbl source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      (tbl, ast) <- codegen modo ex tbl
      return $ Just (tbl, ast)

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule Map.empty . show >>= return . fmap fst

repl :: IO ()
repl = runInputT defaultSettings (loop Map.empty initModule)
  where
  loop tbl mod = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- liftIO $ process mod tbl input
        case modn of
          Just (modn, tbl) -> loop tbl modn
          Nothing -> loop tbl mod

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname >> return ()
