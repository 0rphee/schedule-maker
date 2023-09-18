module Main (main) where

import CmdLineOpts
import Data.Yaml (decodeFileEither)
import Options.Applicative
import Test.Tasty.Bench
import Validation

-- testRunExe :: [String] -> IO ()
testRunExe args = do
  let (NormalOptions yamlSource prettyPrintToStdout ouputFilePath) = case execParserPure
        defaultPrefs
        options
        args of
        Success a -> a
        _ -> error "Error parsing cmdline options"

  res <- decodeFileEither yamlSource -- "test-english.yaml"
  case res of
    Right result -> do
      let a = collectValidationResults result
      pure a

main :: IO ()
main =
  defaultMain
    [ bench "validation" $ nfIO (testRunExe ["test-english.yaml", "-p", "-o", "schedules.xlsx"])
    ]