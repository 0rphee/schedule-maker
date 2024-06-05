{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module WriteiCal (saveICal) where

import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Default
import Data.Function ((&))
import Data.Map.Strict as M
import Data.Text qualified as TS
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Time.Calendar.Easter (sundayAfter)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Text.ICalendar hiding (Class)
import Types

emptyVCalendar :: VCalendar
emptyVCalendar = def

emptyVEvent :: VEvent
emptyVEvent =
  VEvent
    { veDTStamp = DTStamp (UTCTime (ModifiedJulianDay 1) (secondsToDiffTime 1)) def, -- date & time of creation
      veUID = UID "" def,
      veClass = def,
      veDTStart = def,
      veCreated = def,
      veDescription = def,
      veGeo = def,
      veLastMod = def,
      veLocation = def,
      veOrganizer = def,
      vePriority = def,
      veSeq = def,
      veStatus = def,
      veSummary = def,
      veTransp = def,
      veUrl = def,
      veRecurId = def,
      veRRule = def,
      veDTEndDuration = def,
      veAttach = def,
      veAttendee = def,
      veCategories = def,
      veComment = def,
      veContact = def,
      veExDate = def,
      veRStatus = def,
      veRelated = def,
      veResources = def,
      veRDate = def,
      veAlarms = def,
      veOther = def
    }

toVCal :: Day -> [IDandSubj] -> VCalendar
toVCal weekStartDay subjects =
  emptyVCalendar
    { vcEvents = vEventMap
    }
  where
    vEventMap :: Map (TL.Text, Maybe (Either Date DateTime)) VEvent
    vEventMap = ((\(txt, ev) -> ((txt, Nothing), ev)) <$> vEventList) & M.fromList

    vEventList :: [(TL.Text, VEvent)]
    vEventList = concatMap idandsubjToVEvents subjects

    idandsubjToVEvents :: IDandSubj -> [(TL.Text, VEvent)]
    idandsubjToVEvents (IDandSubj (subId, subj)) = fmap (classToEvent subId subj.subjName subj.subjProfessor) subj.subjclasses

    classToEvent :: TS.Text -> TS.Text -> TS.Text -> Class -> (TL.Text, VEvent) -- T.Text: UID value
    classToEvent subId name teacher individualClass =
      ( uidText,
        emptyVEvent
          { veSummary =
              Just $
                Summary
                  { summaryValue = TL.fromStrict (name <> "(" <> subId <> ")"),
                    summaryLanguage = def,
                    summaryAltRep = def,
                    summaryOther = def
                  },
            veUID = UID uidText def,
            veDTStart = Just startDatetime,
            veDTEndDuration = Just $ Left endDatetime,
            veDescription =
              Just $
                Description
                  { descriptionValue = TL.fromStrict teacher,
                    descriptionLanguage = def,
                    descriptionAltRep = def,
                    descriptionOther = def
                  }
          }
      )
      where
        uidText :: TL.Text
        uidText = TL.fromStrict subId <> TL.pack (show $ getClassDayOffset individualClass)

        dayOfClass :: Day
        dayOfClass = addDays (getClassDayOffset individualClass) weekStartDay

        startDatetime :: DTStart
        startDatetime =
          DTStartDateTime
            { dtStartDateTimeValue = buildDateTime individualClass.classInterval.intervalStartingTime,
              dtStartOther = def
            }
        endDatetime :: DTEnd
        endDatetime =
          DTEndDateTime
            { dtEndDateTimeValue = buildDateTime individualClass.classInterval.intervalEndTime,
              dtEndOther = def
            }
        buildDateTime :: Time -> DateTime
        buildDateTime time =
          FloatingDateTime $
            LocalTime
              { localDay = dayOfClass,
                localTimeOfDay =
                  TimeOfDay
                    { todHour = fromEnum time.timeHour,
                      todMin = minuteToInt time.timeMinute,
                      todSec = 0
                    }
              }

getLocalTime :: IO LocalTime
getLocalTime = do
  utcTime <- systemToUTCTime <$> getSystemTime
  timezone <- getTimeZone utcTime
  pure $ utcToLocalTime timezone utcTime

renderICal :: [IDandSubj] -> IO BSL.ByteString
renderICal idAndSubj = do
  (LocalTime today _) <- getLocalTime
  let nextMonday = addDays 1 $ sundayAfter today
  let vcal = toVCal nextMonday idAndSubj
  pure $ printICalendar def vcal


saveICal :: [IDandSubj] -> FilePath -> IO ()
saveICal idAndSubj filepath = do
  renderedICal <- renderICal idAndSubj
  BSL.writeFile filepath renderedICal
