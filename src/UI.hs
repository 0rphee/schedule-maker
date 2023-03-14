module UI where

import Lib

import Brick.Main (simpleMain)
import Brick.Widgets.Core
import Brick (Widget)
import Brick.Widgets.Border

ui :: Widget ()
ui = undefined

data State = State
  {
    allAvailableSubjects :: [Subject]
  , compatibleSubjects :: [Subject]
  }


main :: IO ()
main = simpleMain ui

