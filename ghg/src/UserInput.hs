{-# LANGUAGE RecordWildCards #-}

module UserInput (eventHandler) where

import Prelude hiding (Right, Left)
import Lib hiding (Rotate)
import Debug.Trace

data Direction = Left | Right deriving Show

data Move = Rotate Direction
          | Move Direction
          | FastDrop
          | WordInput Char
          | Start
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
getMove (EventKey (Char 's') Down _ _) = Just Start
getMove (EventKey (Char c) Down _ _) = Just (WordInput c)
getMove _ = Nothing

update :: Maybe Move -> Game -> Game
update x y = traceShow (x, y) y
update (Just x) g@Play{} = 
  case x of
    Rotate Right -> traceShow (fall g) g
    _ -> g
update Nothing g = g