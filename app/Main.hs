module Main (main) where

import CmdLineOpts
import Validation (runProgLogic)

main :: IO ()
main = do
  putStrLn "hlint and fourmolu 333333"
  opts <-
    execParser options
  runProgLogic opts
