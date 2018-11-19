module UserInput (eventHandler) where

import Lib

eventHandler :: Event -> Game -> Game
eventHandler e g = g