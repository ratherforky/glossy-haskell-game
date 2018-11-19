module Renderer (game2Pic) where

import Lib

blockSize :: Float
blockSize = 10


game2Pic :: Game -> Picture
game2Pic (Menu x) = rendMenu x
game2Pic (Play {for = f,back = b, fall = p}) = Pictures (rendBack b ++ rendFor f ++ rendFall p)

rendMenu :: Menu -> Picture
rendMenu (M i) = Blank

rendBack :: Background -> [Picture]
rendBack (Background ps) = arrayBasic (fmap (fmap f) ps) where
  f :: (Maybe Char, Int) -> Picture
  f (_, x) = Color (makeColor 1 1 1 (g x)) (Polygon [(0,0),(0,blockSize),(blockSize,blockSize),(blockSize,0)])
  g :: Int -> Float
  g x = exp (-(((fromIntegral x) + 1)/ 3))

rendFor :: Foreground -> [Picture]
rendFor (Foreground ps) = arrayBasic (fmap (fmap f) ps) where
  f :: Maybe Tetramino -> Picture
  f (Just x) = rendTetramino x (Polygon [(0,0),(0,blockSize),(blockSize,blockSize),(blockSize,0)])
  f Nothing  = Blank

rendFall :: FallingBlock -> [Picture]
rendFall (FallingBlock t i is) = [Blank]

--makeColor red green blue alpha

rendTetramino :: Tetramino -> Picture -> Picture
rendTetramino I p = Color (makeColor 0 1 1 1) p
rendTetramino O p = Color (makeColor 1 1 0 1) p
rendTetramino T p = Color (makeColor 1 0 1 1) p
rendTetramino S p = Color (makeColor 0 1 0 1) p
rendTetramino Z p = Color (makeColor 1 0 0 1) p
rendTetramino J p = Color (makeColor 0 0 1 1) p
rendTetramino L p = Color (makeColor 1 0.5 0 1) p

arrayBasic :: [[Picture]] -> [Picture]
arrayBasic = foldr f [] where
  f :: [Picture] -> [Picture] -> [Picture]
  f xs ps = (fmap (Translate 0 blockSize) ps) ++ (foldr remBlankFold [] (zip [0..] xs))

remBlankFold :: (Float,Picture) -> [Picture] -> [Picture]
remBlankFold (_,Blank) ps = ps
remBlankFold (f,p)     ps = (Translate f 0 p):(ps)
