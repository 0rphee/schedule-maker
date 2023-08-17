{-# LANGUAGE LambdaCase #-}

module Main (main) where

import CmdLineOpts
import Control.Monad (when)
import Data.Yaml.Aeson
import PPrint
import Prettyprinter
import Prettyprinter.Render.Terminal
import System.Console.Terminal.Size
import System.IO (stdout)
import Types
import Validation (collectValidationResults)
import WriteXlsx (saveExcel)

runExe :: IO ()
runExe = do
  (Options {yamlSource, prettyPrintToStdout, outputFilePath}) <-
    execParser options
  (res :: Either ParseException [IDandSubj]) <-
    decodeFileEither yamlSource -- "test-english.yaml"
  sz <-
    size >>= \case
      Nothing -> pure 80
      Just (Window _ w) -> pure w

  let layout = LayoutOptions (AvailablePerLine sz 1)
      prettyRender a = renderIO stdout $ layoutSmart layout a

  case res of
    Left err -> putStrLn $ prettyPrintParseException err -- yaml parsing errors
    Right result -> case collectValidationResults result of
      Left errs -> prettyRender (annotateErrors errs) -- validation errors
      Right lists -> do
        when prettyPrintToStdout $ prettyRender (annotateSubjectLists lists)
        saveExcel lists outputFilePath

main :: IO ()
main = runExe
