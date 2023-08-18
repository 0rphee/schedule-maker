module Main (main) where

import CmdLineOpts
import Options.Applicative
import Validation

testRunExe :: IO ()
testRunExe = do
  let opts = case execParserPure
        defaultPrefs
        options
        ["test-english.yaml", "-p", "-o", "schedules.xlsx"] of
        Success a -> a
        _ -> error "Error parsing cmdline options"
  runProgLogic opts

main :: IO ()
main = do
  putStrLn "Open 'schedules.xslx' to check program output"
  testRunExe
