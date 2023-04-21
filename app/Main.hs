module Main (main) where

import Lib ( Options (..), execParser, options, program )

main :: IO ()
main = do
  (Options (langMode, filePath)) <- execParser options
  program langMode filePath

