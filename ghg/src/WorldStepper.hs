module WorldStepper (worldStepper) where

import Lib
import Data.Maybe

interval :: Float
interval = 0.5

hasCollided :: Fg -> [(Int, Int)] -> Bool
hasCollided (Fg tetras) = foldr f k
  where
    k = False
    f _ True = True
    f (row, col) False
      | row > worldHeight = True
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
newFallingBlock r = FallingBlock ([minBound..maxBound] !! ((floor r) `mod` 7))
                                 (0, 5)
                                 North

fallBlock :: FallingBlock -> FallingBlock
fallBlock (FallingBlock tet (y, x) rot) = FallingBlock tet (y+1, x) rot

worldStepper :: Float -> Game -> Game
worldStepper dt (Menu menu) = Menu menu
worldStepper dt game
  | (accTime game) + dt < interval = game { accTime = (accTime game) + dt }
  | otherwise = game { for = for''
                     , fall = chosenBlock
                     , accTime = 0
                     , rands = rands' }
  where
    (Play for' back' wtf' fall' _ (r:rands') accTime') = game
    
    pos = blockPoints fall''
    fall'' = fallBlock fall'
    collided = hasCollided for' (pos)

    chosenBlock :: FallingBlock
    chosenBlock = if collided
                    then newFallingBlock r -- Make new falling block
                    else fall''
                             
    for'' = if collided
               then moveToForeground fall' for'
               else for'
               
    

