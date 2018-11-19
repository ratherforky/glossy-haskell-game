{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

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
-- getMove (EventKey (Char '(' Down _ _) = Just Start
getMove (EventKey (Char c) Down _ _) = Just (WordInput c)
getMove _ = Nothing

rotateCompass Right North = East
rotateCompass Right East = South
rotateCompass Right South = West
rotateCompass Right West = North
rotateCompass Left n = rotateCompass Right . rotateCompass Right . rotateCompass Right $ n

movePoint Right c = addPointPoint c (0, 1)
movePoint Left c = addPointPoint c (0, -1)


update :: Maybe Move -> Game -> Game
-- update x y = traceShow (x, y) y
update (Just FastDrop) g@Menu{} = traceShow "Hey we're doing a thing now." $ initial_game 10 10
update (Just x) g@Play{fall = FallingBlock t c r} =
  case traceShowId x of
    Rotate d -> changeIfNoCollision (FallingBlock t c (rotateCompass d r)) g
    Move d -> changeIfNoCollision (FallingBlock t (movePoint d c) r) g
    FastDrop -> g -- Bump the tick rate by some scaler
    WordInput c -> g { word = (word g) ++ [c] }
    _ -> g
update _ g = g

changeIfNoCollision :: FallingBlock -> Game -> Game
changeIfNoCollision b g | hasCollided (for g) b = g
                        | wallCollision b       = g
                        | otherwise             = g {fall = b}
