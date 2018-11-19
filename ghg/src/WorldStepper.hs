module WorldStepper (worldStepper) where

import Lib
import Data.Maybe

speed :: Int
speed = 2

worldHeight :: Int
worldHeight = 18

worldWidth :: Int
worldWidth = 10

hasCollided :: Foreground -> [(Int, Int)] -> Bool
hasCollided (Foreground tetras) = foldr f k
  where
    k = False
    f _ True = True
    f (col, row) False
      | col < 0 || col >= worldHeight = False
      | row >= worldHeight = False
      | otherwise = isJust ((tetras !! col) !! row)

worldStepper :: Float -> Game -> Game
worldStepper dt (Menu menu) = Menu menu
worldStepper dt game = game { for = for'
                            , back = back'
                            , fall = fall'
                            , accTime = accTime' }
  where
    (Play for' back' wtf' fall' accTime') = game
    
    fall'' = fall' { tetShape = chosenTetShape }

    chosenTetShape = if hasCollided for' tetShape''
                        then tetShape fall'
                        else tetShape''
                             
    tetShape'' = [(col+1, row) | (col, row) <- tetShape fall']
    acc = accTime game + dt

