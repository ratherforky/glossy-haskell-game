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
    f (row, col) False
      | row >= worldHeight = True
      | otherwise = elem (row, col) (map fst tetras)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x > y = y:(merge (x:xs) ys)
  | otherwise = x:(merge xs (y:ys))

--Assumes both lists sorted
moveToForeground :: FallingBlock -> Fg -> Fg
moveToForeground fb@(FallingBlock tetra _ _) (Fg ts) =
  Fg $ merge (zip (blockPoints fb) (repeat tetra)) ts


-- TODO: ADD TO THIS
newFallingBlock :: Float -> FallingBlock
newFallingBlock r = FallingBlock ([minBound..maxBound] !! (floor 7*r))
                                 (0, 5)
                                 North

worldStepper :: Float -> Game -> Game
worldStepper dt (Menu menu) = Menu menu
worldStepper dt game
  | (accTime game) + dt < interval = game { accTime = (accTime game) + dt }
  | otherwise = game { for = for''
                     , fall = fall''
                     , accTime = 0 }
  where
    (Play for' back' wtf' fall' accTime' rands') = game
    
    fall'' = fall' { tetShape = chosenTetShape }
    collided = hasCollided for' tetShape''
    chosenTetShape = if collided
                        then tetShape fall' -- Make new falling block
                        else tetShape''
                             
    for'' = if collided
               then moveToForeground fall' for'
               else for'
                    
    tetShape'' = [(row+1, col) | (row, col) <- tetShape fall']

