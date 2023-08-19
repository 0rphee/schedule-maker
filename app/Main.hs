module Main (main) where

import CmdLineOpts
import Validation (runProgLogic)

main :: IO ()
main = do
  putStrLn "hello world please windows"
  opts <-
    execParser options
  runProgLogic opts
