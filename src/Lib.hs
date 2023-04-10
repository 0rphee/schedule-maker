{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib (someFunc) where

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
  (res :: Either ParseException [(T.Text, Subject, [Class])]) <- decodeFileEither "test.yaml"
  case res of
    Left err     -> putStrLn $ prettyPrintParseException err
    Right result -> print $ convertValues result >>= runValidation

runValidation :: (M.Map T.Text Subject, [Class]) -> Either Error String -- TODO: fix 
runValidation (valMap, classes) 
  = case validateClasses classes of
      Just err -> Left err
      Nothing  -> Right "Yay"

convertValues :: [(T.Text, Subject, [Class])] -> Either Error (M.Map T.Text Subject, [Class])
convertValues values = go values (Right (M.empty, []))
  where go :: [(T.Text, Subject, [Class])] -> Either Error (M.Map T.Text Subject, [Class]) -> Either Error (M.Map T.Text Subject, [Class])
        go [] result = result
        go ((id', subj, cls):xs) res
          = do
          (valMap, classList) <- res
          case M.lookup id' valMap of
            Just val -> Left $ RepeatedSubjId id' subj val
            Nothing -> go xs (Right (M.insert id' subj valMap, cls <> classList))
        
data Class
  = MondayClass    {classSubjId :: T.Text, classInterval :: Interval}
  | TuesdayClass   {classSubjId :: T.Text, classInterval :: Interval}
  | WednesdayClass {classSubjId :: T.Text, classInterval :: Interval}
  | ThursdayClass  {classSubjId :: T.Text, classInterval :: Interval}
  | FridayClass    {classSubjId :: T.Text, classInterval :: Interval}
  | SaturdayClass  {classSubjId :: T.Text, classInterval :: Interval}
  | SundayClass    {classSubjId :: T.Text, classInterval :: Interval}

instance Show Class where
  show x 
    = case x of
        MondayClass    _ inter -> "Monday: " <> show inter
        TuesdayClass   _ inter -> "Monday: " <> show inter
        WednesdayClass _ inter -> "Monday: " <> show inter
        ThursdayClass  _ inter -> "Monday: " <> show inter
        FridayClass    _ inter -> "Monday: " <> show inter
        SaturdayClass  _ inter -> "Monday: " <> show inter
        SundayClass    _ inter -> "Monday: " <> show inter
      

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
     }
instance Show Subject where
  show (MkSubject sname sprof) = "{ Subject name: " <> show sname 
                              <> ", Subject professor: " <> show sprof  <> " }"

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

classesOverlap :: Class -> Class -> Bool
classesOverlap class1 class2 
  = case (class1, class2) of
      (MondayClass _ inter1, MondayClass _ inter2)       -> intervalsOverlap inter1 inter2
      (TuesdayClass _ inter1, TuesdayClass _ inter2)     -> intervalsOverlap inter1 inter2
      (WednesdayClass _ inter1, WednesdayClass _ inter2) -> intervalsOverlap inter1 inter2
      (ThursdayClass _ inter1, ThursdayClass _ inter2)   -> intervalsOverlap inter1 inter2
      (FridayClass _ inter1, FridayClass _ inter2)       -> intervalsOverlap inter1 inter2
      (SaturdayClass _ inter1, SaturdayClass _ inter2)   -> intervalsOverlap inter1 inter2
      _                                              -> False
      

validateClasses :: [Class] -> Maybe Error
validateClasses allClasses = foldl f Nothing combinations
  where combinations = toTup <$> tuples 2 allClasses
        toTup (c1:c2:_) = (c1, c2)
        f :: Maybe Error-> (Class, Class) -> Maybe Error
        f Nothing (c1, c2)= if classesOverlap c1 c2
                            then Just $ OverlappingClasses c1 c2
                            else Nothing
        f err _ = err

          


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

instance {-# OVERLAPPING #-} FromJSON (T.Text -> Class) where
  parseJSON (Object obj) = prependFailure "parsing Class failed, " $ do
    day      <- obj .: "dia" 
    interval <- prependFailure ("in day '" <> T.unpack day <> "', ") $ parseJSON (Object obj)
    case day of
      "lunes"     -> pure $ \sid -> MondayClass sid interval
      "martes"    -> pure $ \sid -> TuesdayClass sid interval
      "miercoles" -> pure $ \sid -> WednesdayClass sid interval
      "jueves"    -> pure $ \sid -> ThursdayClass sid interval
      "viernes"   -> pure $ \sid -> FridayClass sid interval
      _           -> parseFail $ "Invalid Class day: " <> T.unpack day
  parseJSON invalid =
        prependFailure "parsing Interval failed, "
            (typeMismatch "Object" invalid) 


instance  {-# OVERLAPPING #-} FromJSON (T.Text, Subject, [Class]) where
  parseJSON (Object obj) = prependFailure "parsing Subject failed, " $ do
    name      <- obj .: "nombre"
    let errorInClassName = "in class '" <> T.unpack name <> "', "
    classId   <- prependFailure errorInClassName $ obj .: "id-clase"
    let errorInClassId = errorInClassName 
                      <> "with ID '" <> T.unpack classId <> "', " 
    professor <- prependFailure errorInClassId $ obj .: "profesor"
    let errorInClassProfessor = errorInClassId 
                              <> ("with Professor: '" <> T.unpack professor <> "', ")
    (classes' :: [T.Text -> Class]) <- prependFailure errorInClassProfessor $ obj .: "dias"
    let classes = fmap (\f -> f classId) classes'
    
    pure (classId, MkSubject name professor, classes)

    
  parseJSON invalid =
        prependFailure "parsing Interval failed, "
            (typeMismatch "Object" invalid) 
