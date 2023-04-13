{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Lib (someFunc) where

import Combinatorics    ( tuples )

import Data.Aeson.Types ( parseFail, prependFailure, typeMismatch )
import Data.Bifunctor   ( Bifunctor (first) )
import Data.Map.Strict  qualified as M
import Data.Text        qualified as T
import Data.Yaml        ( FromJSON (..), ParseException, Value (..),
                          decodeFileEither, prettyPrintParseException, (.:) )

data Class
  = MondayClass { classInterval :: Interval
                }
  | TuesdayClass { classInterval :: Interval
                 }
  | WednesdayClass { classInterval :: Interval
                   }
  | ThursdayClass { classInterval :: Interval
                  }
  | FridayClass { classInterval :: Interval
                }
  | SaturdayClass { classInterval :: Interval
                  }
  | SundayClass { classInterval :: Interval
                }

instance Show Class where
  show x
    = case x of
        MondayClass    inter -> "Monday: " <> show inter
        TuesdayClass   inter -> "Tuesday: " <> show inter
        WednesdayClass inter -> "Wednesday: " <> show inter
        ThursdayClass  inter -> "Thursday: " <> show inter
        FridayClass    inter -> "Friday: " <> show inter
        SaturdayClass  inter -> "Saturday: " <> show inter
        SundayClass    inter -> "Sunday: " <> show inter


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
  deriving (Eq, Ord)

instance Show Hour where
 show x
  = case x of
    H0  -> "00"
    H1  -> "01"
    H2  -> "02"
    H3  -> "03"
    H4  -> "04"
    H5  -> "05"
    H6  -> "06"
    H7  -> "07"
    H8  -> "08"
    H9  -> "09"
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

data Minute
  = ZeroMinutes
  | HalfAnHour
  deriving (Eq, Ord)

instance Show Minute where
  show x
    = case x of
      ZeroMinutes -> "00"
      HalfAnHour  -> "30"

data Time
  = MkTime { timeHour   :: Hour
           , timeMinute :: Minute
           }
  deriving (Eq, Ord)

instance Show Time where
  show (MkTime hour minutes) = show hour <> ":" <> show minutes


data Interval
  = MkInterval { intervalStartingTime :: Time
               , intervalEndTime      :: Time
               }

instance Show Interval where
  show (MkInterval beginning end) = show beginning <> "-" <> show end

data Subject
  = MkSubject { subjName      :: T.Text
              , subjProfessor :: T.Text
              , subjclasses   :: [Class]
                -- maybe will change to S.Seq
              }

instance Show Subject where
  show (MkSubject sname sprof classes) = "{ Subject name: " <> show sname
                              <> ", Subject professor: " <> show sprof  <> ", Subject classes "<> show classes <>  " }"

data Error
  = OverlappingClasses Class Class
  | RichOverlappingClasses (Subject, Class) (Subject, Class)
  | RepeatedSubjId T.Text Subject Subject
  deriving (Show)

createInterval :: Time -> Time -> Maybe Interval
createInterval x y = if x < y
                     then Just $ MkInterval x y
                     else Nothing
instance FromJSON Time where
  parseJSON (String str)
     = prependFailure "parsing Time failed, " $ case res of
       [x,y] -> MkTime <$> h x <*> t y
       []    -> parseFail "inexistent hour for class"
       _     -> parseFail "unexpected ':'. More than 1."
    where res =  T.splitOn ":" str
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
                      _    -> parseFail $ T.unpack $ "Invalid hour: " <> head'
          t tail' = case tail' of
                      "00" -> pure ZeroMinutes
                      "30" -> pure HalfAnHour
                      _    -> parseFail $ T.unpack $ "Invalid minutes: " <> tail'
  parseJSON invalid =
        prependFailure "parsing Time failed, "
            (typeMismatch "String" invalid)

instance FromJSON Interval where
  parseJSON (Object obj) = do
    beginningTime <- obj .: "inicio"
    endTime       <- obj .: "final"
    case createInterval beginningTime endTime of
      Nothing              ->
                  parseFail $ "invalid Interval, class ends: "
                            <> show beginningTime
                            <> ", before it begins: "
                            <> show endTime
      Just validInterval -> pure validInterval
  parseJSON invalid =
        prependFailure "parsing Interval failed, "
            (typeMismatch "Object" invalid)

instance {-# OVERLAPPING #-} FromJSON Class where
  parseJSON (Object obj) = prependFailure "parsing Class failed, " $ do
    day      <- obj .: "dia"
    interval <- prependFailure (T.unpack $ "in day '" <> day <> "', ") $ parseJSON (Object obj)
    case day of
      "lunes"     -> pure $ MondayClass interval
      "martes"    -> pure $ TuesdayClass interval
      "miercoles" -> pure $ WednesdayClass interval
      "jueves"    -> pure $ ThursdayClass interval
      "viernes"   -> pure $ FridayClass interval
      _           -> parseFail $ "Invalid Class day: " <> T.unpack day
  parseJSON invalid =
        prependFailure "parsing Interval failed, "
            (typeMismatch "Object" invalid)

instance  {-# OVERLAPPING #-} FromJSON (T.Text, Subject) where
  parseJSON (Object obj) = prependFailure "parsing Subject failed, " $ do
    name      <- obj .: "nombre"
    let errorInClassName = "in class '" <> T.unpack name <> "', "
    classId   <- prependFailure errorInClassName $ obj .: "id-clase"
    let errorInClassId = errorInClassName
                      <> "with ID '" <> T.unpack classId <> "', "
    professor <- prependFailure errorInClassId $ obj .: "profesor"
    let errorInClassProfessor = errorInClassId
                              <> ("with Professor: '" <> T.unpack professor <> "', ")
    classes <- prependFailure errorInClassProfessor $ obj .: "dias"
    pure (classId, MkSubject name professor classes)

  parseJSON invalid =
        prependFailure "parsing Interval failed, "
            (typeMismatch "Object" invalid)

intervalsOverlap :: Interval -> Interval -> Bool
intervalsOverlap (MkInterval a b) (MkInterval x y)
  | a == x && b == y = True
  | a < x && x < b   = True
  | a < y && y < b   = True
  | x < a && a < y   = True
  | x < b && b < y   = True
  | otherwise        = False

classesOverlap :: Class -> Class -> Bool
classesOverlap class1 class2
  = case (class1, class2) of
      (MondayClass inter1, MondayClass inter2)       -> intervalsOverlap inter1 inter2
      (TuesdayClass inter1, TuesdayClass inter2)     -> intervalsOverlap inter1 inter2
      (WednesdayClass inter1, WednesdayClass inter2) -> intervalsOverlap inter1 inter2
      (ThursdayClass inter1, ThursdayClass inter2)   -> intervalsOverlap inter1 inter2
      (FridayClass inter1, FridayClass inter2)       -> intervalsOverlap inter1 inter2
      (SaturdayClass inter1, SaturdayClass inter2)   -> intervalsOverlap inter1 inter2
      _                                                  -> False

validateClasses :: [Class] -> Maybe Error
validateClasses allClasses = foldl f Nothing combinations
  where combinations = toTup <$> tuples 2 allClasses
        toTup xs
          = case xs of
              (c1:c2:_) -> (c1, c2)
              _ -> error "This should never happen (list of more than 2 elements for combinations)"
        f :: Maybe Error -> (Class, Class) -> Maybe Error
        f Nothing (c1, c2) = if classesOverlap c1 c2
                             then Just $ OverlappingClasses c1 c2
                             else Nothing
        f err _ = err

runValidation :: (M.Map T.Text Subject, [Class]) -> Either Error String -- TODO: fix
runValidation (valMap, classes)
  = case validateClasses classes of
      Nothing  -> Right "Everything alright!"  -- TODO: improve later
      Just err -> case err of
                   OverlappingClasses c1 c2 ->
                    let (id1, id2) = (classSubjId c1, classSubjId c2)
                     in Left $ RichOverlappingClasses (valMap M.! id1, c1) (valMap M.! id2, c2) -- using the non-total function because the subject is sure to be found.
                   _ -> error "This should never happen (class id not found in Map of subjects)"

convertValues :: [(T.Text, Subject, [Class])] -> Either Error (M.Map T.Text Subject, [Class])
convertValues values = go values (Right (M.empty, []))
  where go :: [(T.Text, Subject, [Class])] -> Either Error (M.Map T.Text Subject, [Class]) -> Either Error (M.Map T.Text Subject, [Class])
        go [] result = result
        go ((id', subj, cls):xs) res
          = do
          (valMap, classList) <- res
          case M.lookup id' valMap of
            Just repeatedSubj -> Left $ RepeatedSubjId id' subj repeatedSubj
            Nothing -> go xs (Right (M.insert id' subj valMap, cls <> classList))

someFunc :: IO ()
someFunc = do
  (res :: Either ParseException [(T.Text, Subject, [Class])]) <- decodeFileEither "test.yaml"
  case res of
    Left err     -> putStrLn $ prettyPrintParseException err
    Right result -> print $ convertValues result >>= runValidation

