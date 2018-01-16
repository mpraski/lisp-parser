{-# OPTIONS_GHC -fno-warn-tabs #-}

module Repl where

import           Control.Monad.Except
import           System.IO
import           Analyser

-- Define the repl function
repl :: IO ()
repl = hSetBuffering stdin LineBuffering >> runRepl

runRepl :: IO ()
runRepl = basis >>= until_ (== ":q") (readPrompt "> ") . evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
	result <- prompt
	if pred result
    	then return ()
    	else action result >> until_ pred prompt action

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: LispEnvironment -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ evalExpr (buildExpr $ run lispObject expr) env

evalAndPrint :: LispEnvironment -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn
