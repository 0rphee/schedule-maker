module Main (main) where

import CmdLineOpts
import Control.Monad (when)
import Data.Yaml.Aeson
import Options.Applicative
import PPrint
import Prettyprinter
import Prettyprinter.Render.Terminal
import System.Console.Terminal.Size
import System.IO (stdout)
import Types
import Validation
import WriteXlsx

testRunExe :: IO ()
testRunExe = do
  let (Options {yamlSource, prettyPrintToStdout, outputFilePath}) = case execParserPure
        defaultPrefs
        options
        ["test-english.yaml", "-p", "-o", "schedules.xlxs"] of
        Success a -> a
        _ -> error "Error parsing cmdline options"
  (res :: Either ParseException [IDandSubj]) <-
    decodeFileEither yamlSource -- "test-english.yaml"
  sz <- size
  let layout = (\s -> LayoutOptions (AvailablePerLine s 1)) $
        case sz of
          Nothing -> 80
          Just (Window _ w) -> w
      prettyRender a = renderIO stdout $ layoutSmart layout a

  case res of
    Left err -> putStrLn $ prettyPrintParseException err -- yaml parsing errors
    Right result -> case collectValidationResults result of
      Left errs -> prettyRender (annotateErrors errs) -- validation errors
      Right lists -> do
        when prettyPrintToStdout $ prettyRender (annotateSubjectLists lists)
        saveExcel lists outputFilePath

main :: IO ()
main = do
  putStrLn "Open 'schedules.xslx' to check program output"
  testRunExe
