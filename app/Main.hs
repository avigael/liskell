module Main where

import Liskell.Eval
import Liskell.Parser
import Liskell.Syntax

import Data.Map.Strict as Map
import System.IO

main :: IO ()
main = do
  putStrLn "Welcome to the Liskell REPL! (Press Ctrl-D to quit.)"
  repl Map.empty

repl :: Globals -> IO ()
repl gctx = do
  putStr "Liskell REPL> "
  hFlush stdout
  input <- getLine
  undefined
