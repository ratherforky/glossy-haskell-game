module WorldStepper (worldStepper) where

import Lib

speed :: Int
speed = 2

worldHeight :: Int
worldHeight = 18

hasCollided :: [[Maybe Tetramino]] -> [(Int, Int)] -> Bool
hasCollided tetras = foldr f k
  where
    k = False
    f _ True = True
    f (col, row) False
      | row >= worldHeight = False
      | otherwise = isJust ((tetras !! col) !! row)

worldStepper :: Float -> Game -> Game
worldStepper dt (Menu menu) = Menu menu
worldStepper dt game = game { for = for'
                            , back = back'
                            , fall = fall'
                            , accTime = acc }
  where
    fall' = (fall game) { tetShape = tetShape' }
    tetShape' = [(col+1, row) | (col, row) <- tetShape (fall game)]
    acc = accTime game + dt
