{-# LANGUAGE LambdaCase #-}

module CmdLineOpts (options, Options (..), execParser, ExampleYamlLanguage (..)) where

import Options.Applicative

data Options
  = NormalOptions
      !FilePath --  yamlSource
      !Bool -- prettyPrintToStdout
      !FilePath -- outputFilePath
      !Bool -- create *.ical schedule
  | PrintExampleYaml !ExampleYamlLanguage -- True english, False spanish

data ExampleYamlLanguage
  = English
  | Spanish

options :: ParserInfo Options
options =
  info
    (opts <**> helper)
    ( fullDesc
        <> header
          "schedule-maker - a command-line utility to create your school schedules."
        <> footer
          "still a work in progress, source code here: https://github.com/0rphee/schedule-maker"
        <> progDesc
          "Create an .xlsx file with your school schedules from a .yaml file with your classes."
        <> failureCode 64
    )
  where
    opts = languageParser <|> normalOpts

normalOpts :: Parser Options
normalOpts = NormalOptions <$> yamlPath <*> prettyPrintStdout <*> outputPath <*> writeICal

languageParser :: Parser Options
languageParser =
  PrintExampleYaml
    <$> option
      parseLanguage
      ( long "print-yaml-example"
          <> metavar "LANGUAGE"
          <> help "Language option: 'es' (spanish) or 'en' (english)"
      )
  where
    parseLanguage =
      str >>= \case
        "es" -> pure Spanish
        "en" -> pure English
        _ -> fail "Invalid language. Use 'es' or 'en'."

yamlPath :: Parser FilePath
yamlPath =
  strArgument
    ( metavar "FILENAME"
        <> help "YAML input file"
        <> action "directory"
        <> action "file"
    )

prettyPrintStdout :: Parser Bool
prettyPrintStdout =
  switch
    ( help "Print to stdout the validated schedules"
        <> short 'p'
        <> long "pretty-print"
    )

outputPath :: Parser FilePath
outputPath =
  strOption
    ( metavar "FILENAME"
        <> help "Write output to FILE (.xlsx)"
        <> action "directory"
        <> action "file"
        <> showDefault
        <> value "schedules.xlsx"
        <> long "output"
        <> short 'o'
    )

writeICal :: Parser Bool
writeICal =
  switch
    ( help "Write the schedules to iCal files (schedule1.ics, schedule2.ics)"
        <> long "ical"
        <> short 'i'
    )
