{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib
    ( someFunc
    ) where

import Data.Text as T

data Day 
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

data Hour 
  = H0   | H1   | H2  | H3  | H4  | H5 
  | H6   | H7   | H8  | H9  | H10 | H11
  | H12  | H13  | H14 | H15 | H16 | H17
  | H18  | H19  | H20 | H21 | H22 | H23
  deriving (Eq, Ord)

data Minute 
  = ZeroMinutes
  | HalfAnHour
  deriving (Eq, Ord)

data Time 
  = MkTime
  {
    timeHour   :: Hour
  , timeMinute :: Minute
  }
  deriving Eq

instance Ord Time where
  compare (MkTime x y) (MkTime a b) 
    | x > a = GT
    | x < a = LT
    | otherwise = compare y b
  
 

data Interval
 = MkInterval 
    {
      startingTime :: Time 
    , endTime      :: Time
    }

data Class 
  = MkClass  
    {
      classDay      :: Day
    , classInterval :: Interval
    }


data Subject 
   = MkSubject 
     { 
       subjName    :: T.Text
     , subjClasses :: [Class] 
     }





someFunc :: IO ()
someFunc = putStrLn "someFunc"
