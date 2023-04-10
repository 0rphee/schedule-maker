{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib (someFunc) where

import Data.Vector (Vector, toList) 
import Data.Text qualified  as T
import Data.Map qualified  as M
import Data.Yaml
    ( (.:),
      decodeFileEither,
      FromJSON(..),
      Value(..),
      ParseException, prettyPrintParseException )
import Data.Aeson.Types
    ( typeMismatch, parseFail, prependFailure)
import Combinatorics (tuples)

someFunc :: IO ()
someFunc = do
  (res :: Either ParseException [(T.Text, Subject)]) <- decodeFileEither "test.yaml"
  case res of
    Left err   -> putStrLn $ prettyPrintParseException err
    Right result -> print result

data Class
  = MondayClass    {classInterval :: Interval}
  | TuesdayClass   {classInterval :: Interval}
  | WednesdayClass {classInterval :: Interval}
  | ThursdayClass  {classInterval :: Interval}
  | FridayClass    {classInterval :: Interval}
  | SaturdayClass  {classInterval :: Interval}
  | SundayClass    {classInterval :: Interval}

instance Show Class where
  show x 
    = case x of
        MondayClass    inter -> "Monday: " <> show inter
        TuesdayClass   inter -> "Monday: " <> show inter
        WednesdayClass inter -> "Monday: " <> show inter
        ThursdayClass  inter -> "Monday: " <> show inter
        FridayClass    inter -> "Monday: " <> show inter
        SaturdayClass  inter -> "Monday: " <> show inter
        SundayClass    inter -> "Monday: " <> show inter
      

data Hour
  = H0   | H1   | H2  | H3  | H4  | H5
  | H6   | H7   | H8  | H9  | H10 | H11
  | H12  | H13  | H14 | H15 | H16 | H17
  | H18  | H19  | H20 | H21 | H22 | H23
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
  = MkTime
  {
    timeHour   :: Hour
  , timeMinute :: Minute
  }
  deriving (Eq, Ord)

instance Show Time where
  show (MkTime hour minutes) = show hour <> ":" <> show minutes


data Interval
 = MkInterval
    {
      intervalStartingTime :: Time
    , intervalEndTime      :: Time
    }

instance Show Interval where
  show (MkInterval beginning end) = show beginning <> "-" <> show end

data Subject
   = MkSubject
     {
       subjName      :: T.Text
     , subjProfessor :: T.Text
     , subjClasses   :: [Class]
     }
instance Show Subject where
  show (MkSubject sname {-sid-} sprof sclasses) = "( Sname: " <> show sname 
                                           -- <> ", Sid: " <> show sid 
                                           <> ", Sprof: " <> show sprof 
                                           <> ", Sclasses: " <> show sclasses 
                                           <> ")"

data SubjErrorInfo 
  = SubjInfo 
    {
      conflictingSubjName      :: T.Text
    , conflictingSubjID        :: T.Text
    , conflictingSubjProfessor :: T.Text
    , conflictingSubjClass     :: Class
    }

subjectMap :: M.Map T.Text Subject
subjectMap = M.empty

data Error 
  = OverlappingClasses Class Class 
  | RepeatedSubjId T.Text Subject Subject
  deriving Show

createInterval :: Time -> Time -> Maybe Interval
createInterval x y = if x < y
                     then Just $ MkInterval x y
                     else Nothing

intervalsOverlap :: Interval -> Interval -> Bool
intervalsOverlap (MkInterval a b) (MkInterval x y)
  | a <= x && x <= b = True
  | a <= y && y <= b = True
  | x <= a && a <= y = True
  | x <= b && b <= y = True
  | otherwise        = False

clasesOverlap :: Class -> Class -> Bool
clasesOverlap class1 class2 
  = case (class1, class2) of
      (MondayClass inter1, MondayClass inter2)       -> intervalsOverlap inter1 inter2
      (TuesdayClass inter1, TuesdayClass inter2)     -> intervalsOverlap inter1 inter2
      (WednesdayClass inter1, WednesdayClass inter2) -> intervalsOverlap inter1 inter2
      (ThursdayClass inter1, ThursdayClass inter2)   -> intervalsOverlap inter1 inter2
      (FridayClass inter1, FridayClass inter2)       -> intervalsOverlap inter1 inter2
      (SaturdayClass inter1, SaturdayClass inter2)   -> intervalsOverlap inter1 inter2
      _                                              -> False
      


validateClasses :: [Class] -> Either (Class, Class) [Class]
validateClasses xs = traverse undefined undefined
  where combinations = toTup <$> tuples 2 xs
        toTup (c1:c2:_) = (c1, c2)
          


-- validateIntervals :: [Interval] -> Maybe [Interval]
-- validateIntervals ls = go ls []
--   where go :: [Interval] -> [Interval] -> Maybe [Interval]
--         go rest [] = case rest of
--                        []     -> Nothing
--                        (x:xs) -> go xs [x]
--         go [] accuml = Just accuml
--         go (x:xs) (itemToValidate:validatedItems)
--           = case traverse (intervalsOverlap itemToValidate) (x:xs) of
--               Nothing -> Nothing
--               _       -> go xs (x:itemToValidate:validatedItems)

-- -- assumes, the day of the class is validated beforehand
-- validateDay :: [Class] -> Maybe [Class]
-- validateDay classes = do
--   let intervals = fmap classInterval classes 
--   case validateIntervals intervals of
--     Just _ -> pure classes
--     _      -> Nothing

type ClassColl = ([Class], [Class], [Class], [Class], [Class], [Class], [Class]) 

-- c = MkInterval ( MkTime H10 ZeroMinutes ) (MkTime H11 HalfAnHour)
-- d = MkInterval ( MkTime H10 HalfAnHour ) (MkTime H12 ZeroMinutes)

-- firstI = MkInterval (MkTime H10 ZeroMinutes) (MkTime H12 HalfAnHour)
-- sedonI = MkInterval (MkTime H0 HalfAnHour)  (MkTime H2 ZeroMinutes)

-- t1 = MkInterval (MkTime H12 HalfAnHour)  (MkTime H13 ZeroMinutes)
-- t2 = MkInterval (MkTime H15 ZeroMinutes) (MkTime H16 HalfAnHour)
-- t3 = MkInterval (MkTime H16 HalfAnHour)  (MkTime H17 ZeroMinutes)
-- t4 = MkInterval (MkTime H18 ZeroMinutes) (MkTime H18 HalfAnHour)
-- t5 = MkInterval (MkTime H11 HalfAnHour)  (MkTime H12 ZeroMinutes)

-- c1 = TuesdayClass firstI 
-- c2 = TuesdayClass sedonI
-- c3 = TuesdayClass c
-- c4 = TuesdayClass d


instance FromJSON Time where
  parseJSON (String str) 
     = prependFailure "parsing Time failed, " $ case res of
       [x,y] -> MkTime <$> h x <*> t y 
       [] -> parseFail "inexistent hour for class"
       _ -> parseFail "unexpected ':'. More than 1."
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
                      "11" -> pure H1
                      "12" -> pure H2
                      "13" -> pure H3
                      "14" -> pure H4
                      "15" -> pure H5
                      "16" -> pure H6
                      "17" -> pure H7
                      "18" -> pure H8
                      "19" -> pure H9
                      "20" -> pure H10
                      "21" -> pure H1
                      "22" -> pure H2
                      "23" -> pure H3
                      _    -> parseFail $ "Invalid hour: " <> T.unpack head'
          t tail' = case tail' of
                      "00" -> pure ZeroMinutes
                      "30" -> pure HalfAnHour
                      _    -> parseFail $ "Invalid minutes: " <> T.unpack tail'
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

instance FromJSON Class where
  parseJSON (Object obj) = prependFailure "parsing Class failed, " $ do
    day      <- obj .: "dia" 
    interval <- prependFailure ("in day '" <> T.unpack day <> "', ") $ parseJSON (Object obj)
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
    classes   <- prependFailure errorInClassProfessor   $ obj .: "dias"
    pure (classId, MkSubject name professor classes)

    
  parseJSON invalid =
        prependFailure "parsing Interval failed, "
            (typeMismatch "Object" invalid) 
  parseJSONList (Array (arr :: Vector Value)) 
    = toList <$> traverse parseJSON arr
  parseJSONList invalid =
        prependFailure "parsing Subjects failed, "
            (typeMismatch "Array" invalid) 
