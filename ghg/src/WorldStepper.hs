module WorldStepper (worldStepper) where

import Lib
import Data.Maybe
import Data.List
import Debug.Trace

interval :: Float
interval = 0.5

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
  | (accTime game) + dt < (interval / (acceleration game)) = game { accTime = (accTime game) + dt }
  | otherwise = game { for = for'''
                     , fall = chosenBlock
                     , accTime = 0
                     , rands = rands' }
  where
    (Play for' back' wtf' fall' _ (r:rands') accTime' _) = game

    fall'' = fallBlock fall'
    collided = hasCollided for' (fall'')

    chosenBlock :: FallingBlock
    chosenBlock = if collided
                    then newFallingBlock r -- Make new falling block
                    else fall''

    for'' = if collided
               then moveToForeground fall' for'
               else for'
    for''' = removeFullRows for''

removeFullRows :: Fg -> Fg
removeFullRows (Fg xs) = Fg (fst . foldr f' ([],0) $ ys)  where
  ys = groupBy (f (==)) . sortBy (f compare) $ xs
  f :: (Int -> Int -> b) -> ((Int,Int),Tetramino) -> ((Int,Int),Tetramino) -> b
  f g ((a,_),_) ((b,_),_) = g a b
  f' :: [((Int,Int),Tetramino)] -> ([((Int,Int),Tetramino)], Int) -> ([((Int,Int),Tetramino)], Int)
  f' ps (ts, offset)
    | length ps /= worldWidth = (map (\((y, x), t) -> ((y + offset, x), t)) ps ++ ts, offset)
    | otherwise = (ts, offset + 1)
