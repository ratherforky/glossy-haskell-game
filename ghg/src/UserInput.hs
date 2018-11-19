module UserInput (eventHandler) where

import Prelude hiding (Right, Left)
import Lib hiding (Rotate)
import Debug.Trace

data Direction = Left | Right deriving Show

data Move = Rotate Direction
          | Move Direction
          | FastDrop
          | WordInput Char
          deriving Show

eventHandler :: Event -> Game -> Game
eventHandler e = update (getMove e)

getMove :: Event -> Maybe Move
getMove (EventKey (SpecialKey key) Down _ _) = 
  case key of
    KeyUp    -> Just (Rotate Right)
    KeyDown  -> Just (Rotate Left)
    KeyRight -> Just (Move Right)
    KeyLeft  -> Just (Move Left)
    KeySpace -> Just FastDrop
    _        -> Nothing
getMove (EventKey (Char c) Down _ _) = Just (WordInput c)
getMove _ = Nothing

update :: Maybe Move -> Game -> Game
update (Just x) g = traceShow (Just x) g
update Nothing g = g