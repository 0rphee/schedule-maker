module Main (main) where

import CmdLineOpts
import Options.Applicative
import Validation

testRunExe :: [String] -> IO ()
testRunExe args = do
  let opts = case execParserPure
        defaultPrefs
        options
        args of
        Success a -> a
        _ -> error "Error parsing cmdline options"
  runProgLogic opts

main :: IO ()
main = do
  putStrLn "Open 'schedules.xslx' to check program output"
  testRunExe ["test-english.yaml", "-p", "-o", "schedules.xlsx"]

  putStrLn "Print example spanish yaml file"
  testRunExe ["--print-yaml-example=es"]

  putStrLn "Print example english yaml file"
  testRunExe ["--print-yaml-example=en"]
