module WorldStepper (worldStepper) where

import Lib
import Data.Maybe

interval :: Float
interval = 0.5

worldHeight :: Int
worldHeight = 18

worldWidth :: Int
worldWidth = 10

hasCollided :: Fg -> [(Int, Int)] -> Bool
hasCollided (Fg tetras) = foldr f k
  where
    k = False
    f _ True = True
    f coords False = elem coords (map fst tetras)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x > y = y:(merge (x:xs) ys)
  | otherwise = x:(merge xs (y:ys))

--Assumes both lists sorted
moveToForeground :: FallingBlock -> Fg -> Fg
moveToForeground (FallingBlock tetra _ tetShape) (Fg ts) =
  Fg $ merge (zip tetShape (repeat tetra)) ts

worldStepper :: Float -> Game -> Game
worldStepper dt (Menu menu) = Menu menu
worldStepper dt game
  | (accTime game) + dt < interval = game { accTime = (accTime game) + dt }
  | otherwise = game { for = for'
                     , back = back'
                     , fall = fall''
                     , accTime = 0 }
  where
    (Play for' back' wtf' fall' accTime') = game
    
    fall'' = fall' { tetShape = chosenTetShape }
    collided = hasCollided for' tetShape''
    chosenTetShape = if collided
                        then tetShape fall' -- Make new falling block
                        else tetShape''
                             
    for'' = if collided
               then moveToForeground fall' for'
               else for'
                    
    tetShape'' = [(col+1, row) | (col, row) <- tetShape fall']

