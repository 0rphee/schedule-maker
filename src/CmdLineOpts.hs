module CmdLineOpts (options, Options (..), execParser) where

import Options.Applicative

data Options = Options
  { yamlSource :: FilePath
  , prettyPrintToStdout :: Bool
  , outputFilePath :: FilePath
  }

options :: ParserInfo Options
options =
  info
    (opts <**> helper)
    ( fullDesc
        <> header
          "schedule-maker - a command-line utility to create your school schedules."
        <> footer
          "still a work in progress, source code here: https://codeberg.org/0rphee/schedule-maker."
        <> progDesc
          "Create an .xlsx file with your school schedules from a .yaml file with your classes."
        <> failureCode 64
    )
  where
    opts :: Parser Options
    opts = Options <$> yamlPath <*> prettyPrintStdout <*> outputPath

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
        <> help ".xlsx output file"
        <> action "directory"
        <> action "file"
        <> showDefault
        <> value "schedules.xlsx"
        <> long "output"
        <> short 'o'
    )
