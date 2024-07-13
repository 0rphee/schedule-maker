#!/usr/bin/env stack
{- stack script
   --snapshot lts-22.23
   --package bytestring
   --package text
   --package regex-tdfa
   --package regex-posix
   --package regex-base
   --package opt-env-conf
   --extra-dep opt-env-conf-0.0.0.0
   --extra-dep autodocodec-0.2.3.0
   --extra-dep autodocodec-yaml-0.3.0.0
   --extra-dep safe-coloured-text-0.3.0.2
   --extra-dep safe-coloured-text-layout-0.2.0.0
 -}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- import Text.Regex.TDFA --(getAllTextMatches, (=~))

import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Version (Version (..))
import OptEnvConf
import Text.Regex.Base.RegexLike
import Text.Regex.Posix

data Settings = Settings
  { fileToMatch :: FilePath
  , regexString :: ByteString
  , replacementString :: ByteString
  }
  deriving (Show)

instance HasParser Settings where
  settingsParser = subEnv "RDM_REP_" $ do
    fileToMatch <-
      setting
        [ help "The filepath to find the regex"
        , reader str
        , argument
        , env "FILEPATH"
        , metavar "FILEPATH"
        ]
    regexString <-
      setting
        [ help "The regex to match"
        , reader str
        , argument
        , env "REGEX"
        , metavar "REGEX"
        ]
    replacementString <-
      setting
        [ help "The replacement string"
        , reader str
        , argument
        , env "REPLACEMENT"
        , metavar "REPLACEMENT"
        ]
    pure Settings {..}

main :: IO ()
main = do
  settings :: Settings <-
    runSettingsParser (Version [0] []) "Replace text with a regex"
  fileText <- B.readFile settings.fileToMatch
  let regString = settings.regexString :: ByteString
  B.putStrLn regString
  let allMatches = getAllTextMatches (fileText =~ regString) :: [ByteString]
  forM_ allMatches B.putStrLn

  pure ()

-- fun :: (RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target)
--      => source1 -> source -> target
-- fun x r = let
--               make = makeRegexOpts (CompOption True True True True False) (ExecOption True)
--            in match (make r) x
