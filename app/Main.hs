module Main (main) where

import CmdLineOpts
import Validation (runProgLogic)

main :: IO ()
main = do
  putStrLn "test combined worflow"
  opts <-
    execParser options
  runProgLogic opts
