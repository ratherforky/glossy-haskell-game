module UserInput (eventHandler) where

import Lib

data Direction = Left | Right

data Move = Rotate Direction
          | Move Direction
          | FastDrop
          | WordInput Char

eventHandler :: Event -> Game -> Game
eventHandler e = update (getMove e)

getMove :: Event -> (Maybe Move)
getMove = undefined

update :: Maybe Move -> Game -> Game
update = undefined