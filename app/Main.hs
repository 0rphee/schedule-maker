{-# LANGUAGE LambdaCase #-}

module Main (main) where

import CmdLineOpts
import Validation (runProgLogic)

main :: IO ()
main = do
  opts <-
    execParser options
  runProgLogic opts
