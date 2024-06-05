{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Types
  ( Class (..),
    Hour (..),
    Minute (..),
    Time (..),
    Interval (..),
    Subject (..),
    IDandSubj (..),
    Error (..),
    minuteToInt,
    getClassDayOffset,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq
import Data.Aeson.Types (parseFail, prependFailure, typeMismatch)
import Data.Text qualified as T
import Data.Yaml
  ( FromJSON (parseJSON),
    Value (Object, String),
    (.:),
  )
import Prettyprinter (Pretty (pretty), indent, line, vsep, (<+>))

data Class
  = MondayClass
      { classInterval :: !Interval
      }
  | TuesdayClass
      { classInterval :: !Interval
      }
  | WednesdayClass
      { classInterval :: !Interval
      }
  | ThursdayClass
      { classInterval :: !Interval
      }
  | FridayClass
      { classInterval :: !Interval
      }
  | SaturdayClass
      { classInterval :: !Interval
      }
  | SundayClass
      { classInterval :: !Interval
      }

instance NFData Class where
  rnf x = rnf (classInterval x)

instance Show Class where
  show x =
    case x of
      MondayClass inter -> "Monday: " <> show inter
      TuesdayClass inter -> "Tuesday: " <> show inter
      WednesdayClass inter -> "Wednesday: " <> show inter
      ThursdayClass inter -> "Thursday: " <> show inter
      FridayClass inter -> "Friday: " <> show inter
      SaturdayClass inter -> "Saturday: " <> show inter
      SundayClass inter -> "Sunday: " <> show inter
  {-# INLINE show #-}

data Hour
  = H0
  | H1
  | H2
  | H3
  | H4
  | H5
  | H6
  | H7
  | H8
  | H9
  | H10
  | H11
  | H12
  | H13
  | H14
  | H15
  | H16
  | H17
  | H18
  | H19
  | H20
  | H21
  | H22
  | H23
  deriving (Eq, Ord, Enum, Bounded)

instance NFData Hour where
  rnf h = h `seq` ()

instance Show Hour where
  show x =
    case x of
      H0 -> "00"
      H1 -> "01"
      H2 -> "02"
      H3 -> "03"
      H4 -> "04"
      H5 -> "05"
      H6 -> "06"
      H7 -> "07"
      H8 -> "08"
      H9 -> "09"
      H10 -> "10"
      H11 -> "11"
      H12 -> "12"
      H13 -> "13"
      H14 -> "14"
      H15 -> "15"
      H16 -> "16"
      H17 -> "17"
      H18 -> "18"
      H19 -> "19"
      H20 -> "20"
      H21 -> "21"
      H22 -> "22"
      H23 -> "23"
  {-# INLINE show #-}

data Minute
  = ZeroMinutes
  | HalfAnHour
  deriving (Eq, Ord, Bounded)

instance NFData Minute where
  rnf m = m `seq` ()

instance Show Minute where
  show x =
    case x of
      ZeroMinutes -> "00"
      HalfAnHour -> "30"
  {-# INLINE show #-}

data Time = MkTime
  { timeHour :: !Hour,
    timeMinute :: !Minute
  }
  deriving (Eq, Ord, Bounded)

instance NFData Time where
  rnf (MkTime x y) = rnf x `seq` rnf y

instance Show Time where
  show (MkTime hour minutes) = show hour <> ":" <> show minutes

instance Enum Time where
  toEnum i =
    case i of
      0 -> MkTime H0 ZeroMinutes
      1 -> MkTime H0 HalfAnHour
      2 -> MkTime H1 ZeroMinutes
      3 -> MkTime H1 HalfAnHour
      4 -> MkTime H2 ZeroMinutes
      5 -> MkTime H2 HalfAnHour
      6 -> MkTime H3 ZeroMinutes
      7 -> MkTime H3 HalfAnHour
      8 -> MkTime H4 ZeroMinutes
      9 -> MkTime H4 HalfAnHour
      10 -> MkTime H5 ZeroMinutes
      11 -> MkTime H5 HalfAnHour
      12 -> MkTime H6 ZeroMinutes
      13 -> MkTime H6 HalfAnHour
      14 -> MkTime H7 ZeroMinutes
      15 -> MkTime H7 HalfAnHour
      16 -> MkTime H8 ZeroMinutes
      17 -> MkTime H8 HalfAnHour
      18 -> MkTime H9 ZeroMinutes
      19 -> MkTime H9 HalfAnHour
      20 -> MkTime H10 ZeroMinutes
      21 -> MkTime H10 HalfAnHour
      22 -> MkTime H11 ZeroMinutes
      23 -> MkTime H11 HalfAnHour
      24 -> MkTime H12 ZeroMinutes
      25 -> MkTime H12 HalfAnHour
      26 -> MkTime H13 ZeroMinutes
      27 -> MkTime H13 HalfAnHour
      28 -> MkTime H14 ZeroMinutes
      29 -> MkTime H14 HalfAnHour
      30 -> MkTime H15 ZeroMinutes
      31 -> MkTime H15 HalfAnHour
      32 -> MkTime H16 ZeroMinutes
      33 -> MkTime H16 HalfAnHour
      34 -> MkTime H17 ZeroMinutes
      35 -> MkTime H17 HalfAnHour
      36 -> MkTime H18 ZeroMinutes
      37 -> MkTime H18 HalfAnHour
      38 -> MkTime H19 ZeroMinutes
      39 -> MkTime H19 HalfAnHour
      40 -> MkTime H20 ZeroMinutes
      41 -> MkTime H20 HalfAnHour
      42 -> MkTime H21 ZeroMinutes
      43 -> MkTime H21 HalfAnHour
      44 -> MkTime H22 ZeroMinutes
      45 -> MkTime H22 HalfAnHour
      46 -> MkTime H23 ZeroMinutes
      47 -> MkTime H23 HalfAnHour
      _ -> error "out of bounds enum"
  fromEnum t =
    case t of
      MkTime H0 ZeroMinutes -> 0
      MkTime H0 HalfAnHour -> 1
      MkTime H1 ZeroMinutes -> 2
      MkTime H1 HalfAnHour -> 3
      MkTime H2 ZeroMinutes -> 4
      MkTime H2 HalfAnHour -> 5
      MkTime H3 ZeroMinutes -> 6
      MkTime H3 HalfAnHour -> 7
      MkTime H4 ZeroMinutes -> 8
      MkTime H4 HalfAnHour -> 9
      MkTime H5 ZeroMinutes -> 10
      MkTime H5 HalfAnHour -> 11
      MkTime H6 ZeroMinutes -> 12
      MkTime H6 HalfAnHour -> 13
      MkTime H7 ZeroMinutes -> 14
      MkTime H7 HalfAnHour -> 15
      MkTime H8 ZeroMinutes -> 16
      MkTime H8 HalfAnHour -> 17
      MkTime H9 ZeroMinutes -> 18
      MkTime H9 HalfAnHour -> 19
      MkTime H10 ZeroMinutes -> 20
      MkTime H10 HalfAnHour -> 21
      MkTime H11 ZeroMinutes -> 22
      MkTime H11 HalfAnHour -> 23
      MkTime H12 ZeroMinutes -> 24
      MkTime H12 HalfAnHour -> 25
      MkTime H13 ZeroMinutes -> 26
      MkTime H13 HalfAnHour -> 27
      MkTime H14 ZeroMinutes -> 28
      MkTime H14 HalfAnHour -> 29
      MkTime H15 ZeroMinutes -> 30
      MkTime H15 HalfAnHour -> 31
      MkTime H16 ZeroMinutes -> 32
      MkTime H16 HalfAnHour -> 33
      MkTime H17 ZeroMinutes -> 34
      MkTime H17 HalfAnHour -> 35
      MkTime H18 ZeroMinutes -> 36
      MkTime H18 HalfAnHour -> 37
      MkTime H19 ZeroMinutes -> 38
      MkTime H19 HalfAnHour -> 39
      MkTime H20 ZeroMinutes -> 40
      MkTime H20 HalfAnHour -> 41
      MkTime H21 ZeroMinutes -> 42
      MkTime H21 HalfAnHour -> 43
      MkTime H22 ZeroMinutes -> 44
      MkTime H22 HalfAnHour -> 45
      MkTime H23 ZeroMinutes -> 46
      MkTime H23 HalfAnHour -> 47

data Interval = MkInterval
  { intervalStartingTime :: !Time,
    intervalEndTime :: !Time
  }

instance NFData Interval where
  rnf (MkInterval f s) = rnf f `seq` rnf s

instance Show Interval where
  show (MkInterval beginning end) = show beginning <> "-" <> show end
  {-# INLINE show #-}

newtype IDandSubj
  = IDandSubj (T.Text, Subject)
  deriving newtype (NFData)

data Subject = MkSubject
  { subjName :: !T.Text,
    subjProfessor :: !T.Text,
    subjclasses :: ![Class]
    -- maybe will change to S.Seq
  }

instance NFData Subject where
  rnf (MkSubject n p c) = rnf n `seq` rnf p `seq` rnf c

instance Show Subject where
  show (MkSubject sname sprof classes) =
    "{ Subject name: "
      <> show sname
      <> ", Subject professor: "
      <> show sprof
      <> ", Subject classes "
      <> show classes
      <> " }"
  {-# INLINEABLE show #-}

data Error
  = OverlappingClasses !(Subject, Class) !(Subject, Class)
  | RepeatedSubjId !T.Text !Subject !Subject
  deriving (Show)

instance NFData Error where
  rnf (OverlappingClasses l r) = rnf l `seq` rnf r
  rnf (RepeatedSubjId x y z) = rnf x `seq` rnf y `seq` rnf z

createInterval :: Time -> Time -> Maybe Interval
createInterval x y =
  if x < y
    then Just $ MkInterval x y
    else Nothing

instance FromJSON Time where
  parseJSON (String str) =
    prependFailure "parsing Time failed, " $ case res of
      [x, y] -> MkTime <$> h x <*> t y
      [] -> parseFail "inexistent hour for class"
      _ -> parseFail "unexpected ':'. More than 1."
    where
      res = T.splitOn ":" str
      h head' = case head' of
        "00" -> pure H0
        "01" -> pure H1
        "02" -> pure H2
        "03" -> pure H3
        "04" -> pure H4
        "05" -> pure H5
        "06" -> pure H6
        "07" -> pure H7
        "08" -> pure H8
        "09" -> pure H9
        "10" -> pure H10
        "11" -> pure H11
        "12" -> pure H12
        "13" -> pure H13
        "14" -> pure H14
        "15" -> pure H15
        "16" -> pure H16
        "17" -> pure H17
        "18" -> pure H18
        "19" -> pure H19
        "20" -> pure H20
        "21" -> pure H21
        "22" -> pure H22
        "23" -> pure H23
        "1" -> pure H1
        "2" -> pure H2
        "3" -> pure H3
        "4" -> pure H4
        "5" -> pure H5
        "6" -> pure H6
        "7" -> pure H7
        "8" -> pure H8
        "9" -> pure H9
        _ -> parseFail $ T.unpack $ "Invalid hour: " <> head'
      t tail' = case tail' of
        "00" -> pure ZeroMinutes
        "30" -> pure HalfAnHour
        _ -> parseFail $ T.unpack $ "Invalid minutes: " <> tail'
  parseJSON invalid =
    prependFailure
      "parsing Time failed, "
      (typeMismatch "String" invalid)

instance FromJSON Interval where
  parseJSON (Object obj) = do
    beginningTime <- obj .: "inicio" <|> obj .: "start"
    endTime <- obj .: "final" <|> obj .: "end"
    case createInterval beginningTime endTime of
      Nothing ->
        parseFail $
          "invalid Interval, class ends: "
            <> show beginningTime
            <> ", before it begins: "
            <> show endTime
      Just validInterval -> pure validInterval
  parseJSON invalid =
    prependFailure
      "parsing Interval failed, "
      (typeMismatch "Object" invalid)

instance {-# OVERLAPPING #-} FromJSON Class where
  parseJSON (Object obj) = prependFailure "parsing Class failed, " $ do
    day <- obj .: "dia" <|> obj .: "day"
    interval <-
      prependFailure (T.unpack $ "in day '" <> day <> "', ") $ parseJSON (Object obj)
    case day of
      "lunes" -> pure $ MondayClass interval
      "monday" -> pure $ MondayClass interval
      "martes" -> pure $ TuesdayClass interval
      "tuesday" -> pure $ TuesdayClass interval
      "miercoles" -> pure $ WednesdayClass interval
      "wednesday" -> pure $ WednesdayClass interval
      "jueves" -> pure $ ThursdayClass interval
      "thursday" -> pure $ ThursdayClass interval
      "viernes" -> pure $ FridayClass interval
      "friday" -> pure $ FridayClass interval
      "sabado" -> pure $ SaturdayClass interval
      "saturday" -> pure $ SaturdayClass interval
      "domingo" -> pure $ SundayClass interval
      "sunday" -> pure $ SundayClass interval
      _ -> parseFail $ "Invalid Class day: " <> T.unpack day
  parseJSON invalid =
    prependFailure
      "parsing Interval failed, "
      (typeMismatch "Object" invalid)

instance {-# OVERLAPPING #-} FromJSON IDandSubj where
  parseJSON (Object obj) = prependFailure "parsing Subject failed, " $ do
    name <- obj .: "nombre" <|> obj .: "name"
    let errorInClassName = "in class '" <> T.unpack name <> "', "
    classId <-
      prependFailure errorInClassName $ obj .: "id-clase" <|> obj .: "class-id"
    let errorInClassId =
          errorInClassName
            <> "with ID '"
            <> T.unpack classId
            <> "', "
    professor <-
      prependFailure errorInClassId $ obj .: "profesor" <|> obj .: "professor"
    let errorInClassProfessor =
          errorInClassId
            <> ("with Professor: '" <> T.unpack professor <> "', ")
    classes <-
      prependFailure errorInClassProfessor $ obj .: "dias" <|> obj .: "days"
    pure $ IDandSubj (classId, MkSubject name professor classes)
  parseJSON invalid =
    prependFailure
      "parsing Interval failed, "
      (typeMismatch "Object" invalid)

instance Pretty Class where
  pretty (MondayClass interval) = "Monday:   " <+> pretty interval
  pretty (TuesdayClass interval) = "Tuesday:  " <+> pretty interval
  pretty (WednesdayClass interval) = "Wednesday:" <+> pretty interval
  pretty (ThursdayClass interval) = "Thursday: " <+> pretty interval
  pretty (FridayClass interval) = "Friday:   " <+> pretty interval
  pretty (SaturdayClass interval) = "Saturday: " <+> pretty interval
  pretty (SundayClass interval) = "Sunday:   " <+> pretty interval

instance Pretty Hour where
  pretty H0 = "00"
  pretty H1 = "01"
  pretty H2 = "02"
  pretty H3 = "03"
  pretty H4 = "04"
  pretty H5 = "05"
  pretty H6 = "06"
  pretty H7 = "07"
  pretty H8 = "08"
  pretty H9 = "09"
  pretty H10 = "10"
  pretty H11 = "11"
  pretty H12 = "12"
  pretty H13 = "13"
  pretty H14 = "14"
  pretty H15 = "15"
  pretty H16 = "16"
  pretty H17 = "17"
  pretty H18 = "18"
  pretty H19 = "19"
  pretty H20 = "20"
  pretty H21 = "21"
  pretty H22 = "22"
  pretty H23 = "23"

instance Pretty Minute where
  pretty ZeroMinutes = "00"
  pretty HalfAnHour = "30"

instance Pretty Time where
  pretty (MkTime h m) = pretty h <> ":" <> pretty m

instance Pretty Interval where
  pretty (MkInterval start end) = pretty start <+> "-" <+> pretty end

instance Pretty Subject where
  pretty (MkSubject name professor classes) =
    "Subject: "
      <> pretty name
      <> line
      <> "Professor: "
      <> pretty professor
      <> line
      <> "Classes:"
      <> line
      <> indent 2 (vsep (map pretty classes))

instance Pretty Error where
  pretty (OverlappingClasses (subj1, class1) (subj2, class2)) =
    "Overlapping classes:"
      <> line
      <> indent 2 (vsep [pretty subj1 <+> pretty class1, pretty subj2 <+> pretty class2])
  pretty (RepeatedSubjId subjId subj1 subj2) =
    "Repeated subject ID: "
      <> pretty subjId
      <> line
      <> indent 2 (vsep [pretty subj1, pretty subj2])

minuteToInt :: Minute -> Int
minuteToInt = \case
  ZeroMinutes -> 0
  HalfAnHour -> 30

getClassDayOffset :: Class -> Integer
getClassDayOffset = \case
  MondayClass _ -> 0
  TuesdayClass _ -> 1
  WednesdayClass _ -> 2
  ThursdayClass _ -> 3
  FridayClass _ -> 4
  SaturdayClass _ -> 5
  SundayClass _ -> 6
