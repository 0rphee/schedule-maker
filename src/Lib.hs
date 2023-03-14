{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lib where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace

data Class
  = MondayClass    {classInterval :: Interval}
  | TuesdayClass   {classInterval :: Interval}
  | WednesdayClass {classInterval :: Interval}
  | ThursdayClass  {classInterval :: Interval}
  | FridayClass    {classInterval :: Interval}
  | SaturdayClass  {classInterval :: Interval}
  | SundayClass    {classInterval :: Interval}
  deriving (Show)

data Hour
  = H0   | H1   | H2  | H3  | H4  | H5
  | H6   | H7   | H8  | H9  | H10 | H11
  | H12  | H13  | H14 | H15 | H16 | H17
  | H18  | H19  | H20 | H21 | H22 | H23
  deriving (Show, Eq, Ord)

data Minute
  = ZeroMinutes
  | HalfAnHour
  deriving (Show, Eq, Ord)

data Time
  = MkTime
  {
    timeHour   :: Hour
  , timeMinute :: Minute
  }
  deriving (Show, Eq, Ord)

data Interval
 = MkInterval
    {
      startingTime :: Time
    , endTime      :: Time
    }
    deriving (Show)

data Subject
   = MkSubject
     {
       subjName    :: T.Text
     , subjClasses :: [Class]
     }
    deriving (Show)

createInterval :: Time -> Time -> Maybe Interval
createInterval x y = if x < y
                     then Just $ MkInterval x y
                     else Nothing

intervalsOverlap :: Interval -> Interval -> Maybe Interval
intervalsOverlap (MkInterval a b) inter2@(MkInterval x y)
  | a <= x && x <= b = Nothing
  | a <= y && y <= b = Nothing
  | x <= a && a <= y = Nothing
  | x <= b && b <= y = Nothing
  | otherwise      = Just inter2

validateIntervals :: [Interval] -> Maybe [Interval]
validateIntervals ls = go ls []
  where go :: [Interval] -> [Interval] -> Maybe [Interval]
        go rest [] = case rest of
                       []     -> Nothing
                       (x:xs) -> go xs [x]
        go [] accuml = Just accuml
        go (x:xs) (itemToValidate:validatedItems)
          = case traverse (intervalsOverlap itemToValidate) (x:xs) of
              Nothing -> Nothing
              _       -> go xs (x:itemToValidate:validatedItems)

-- assumes, the day of the class is validated beforehand
validateDay :: [Class] -> Maybe [Class]
validateDay classes = do
  let intervals = fmap classInterval classes 
  _ <- validateIntervals intervals
  pure classes


type ClassColl = ([Class], [Class], [Class], [Class], [Class], [Class], [Class]) 

-- createSubject :: T.Text -> [Class] -> Maybe Subject 
-- createSubject name classes = 
--   where go :: [Class] -> ClassColl -> ClassColl
--         go [] a = a
--         go (x:xs) (a, b, c, d, e, f, g) 
--           = case x of
--               MondayClass _    -> (x:a, b, c, d, e, f, g)
--               TuesdayClass _   -> (a, x:b, c, d, e, f, g)
--               WednesdayClass _ -> (a, b, x:c, d, e, f, g)
--               ThursdayClass _  -> (a, b, c, x:d, e, f, g)
--               FridayClass _    -> (a, b, c, d, x:e, f, g)
--               SaturdayClass _  -> (a, b, c, d, e, x:f, g)
--               SundayClass _    -> (a, b, c, d, e, f, x:g)


c = MkInterval ( MkTime H10 ZeroMinutes ) (MkTime H11 HalfAnHour)
d = MkInterval ( MkTime H10 HalfAnHour ) (MkTime H12 ZeroMinutes)

firstI = MkInterval (MkTime H10 ZeroMinutes) (MkTime H12 HalfAnHour)
sedonI = MkInterval (MkTime H0 HalfAnHour)  (MkTime H2 ZeroMinutes)

t1 = MkInterval (MkTime H12 HalfAnHour)  (MkTime H13 ZeroMinutes)
t2 = MkInterval (MkTime H15 ZeroMinutes) (MkTime H16 HalfAnHour)
t3 = MkInterval (MkTime H16 HalfAnHour)  (MkTime H17 ZeroMinutes)
t4 = MkInterval (MkTime H18 ZeroMinutes) (MkTime H18 HalfAnHour)
t5 = MkInterval (MkTime H11 HalfAnHour)  (MkTime H12 ZeroMinutes)

c1 = TuesdayClass firstI 
c2 = TuesdayClass sedonI
c3 = TuesdayClass c
c4 = TuesdayClass d

someFunc :: IO ()
someFunc = TIO.putStrLn "someFunc"



