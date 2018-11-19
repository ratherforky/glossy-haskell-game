{-# LANGUAGE RecordWildCards #-}

module Lib
  ( module Lib
  , module Graphics.Gloss
  , module Graphics.Gloss.Interface.Pure.Game
  , module System.Random
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

worldHeight :: Int
worldHeight = 18

worldWidth :: Int
worldWidth = 10

data Fg = Fg { unFg :: [((Int, Int), Tetramino)] } deriving Show

data Game = Play {for   :: Fg,
                  back  :: Background,
                  wtf   :: Word2Find,
                  fall  :: FallingBlock,
                  word  :: String,
                  rands :: [Float],
                  accTime :: Float,
                  acceleration :: Float}
          | Menu {menu :: Menu,
                  rands :: [Float]
                  }
          deriving Show

initial_game :: [Float] -> Game
initial_game (r:rands) = Play{..}
  where
    for     = Fg []--Foreground ((map . map) (const Nothing) [[1..width] | _ <- [1..height]])
    back    = Background ((map . map) (const (Nothing,0)) [[1..worldWidth] | _ <- [1..worldHeight]])
    wtf     = Word2Find "hello"
    -- fall    = FallingBlock S (0, fromIntegral $ width `div` 2) North
    fall      = newFallingBlock r
    word      = ""
    accTime = 0
    acceleration = 1

-- TODO: ADD TO THIS
newFallingBlock :: Float -> FallingBlock
newFallingBlock r = FallingBlock ([minBound..maxBound] !! ((floor r) `mod` 7))
                                 (0, 5)
                                 North


data Background = Background [[(Maybe Char, Int)]] deriving Show

data Foreground = Foreground [[Maybe Tetramino]] deriving Show

-- initData :: Game
-- --initData = Menu {menu = (M 0)}
-- initData = (Play {for = initArrFor, back = initArrBack, wtf = (Word2Find "Codeword"), fall = (FallingBlock J (0,0) North), word = "Hi", rands = []})

-- initArrFor :: Foreground
-- initArrFor = Foreground ([b,b,b,b,b,b,b,b]) where
--   b = [(Just O),(Just T),(Just S),(Just Z)]

-- initArrBack :: Background
-- initArrBack = Background ([b,b,b,b,b,b,b,b]) where
--   b = [(Nothing,0),(Nothing,0),(Nothing,0),(Nothing,0)]

data Word2Find = Word2Find String deriving Show

data FallingBlock = FallingBlock Tetramino (Float, Float) Rotation deriving Show

data Rotation = North | South | East | West deriving Show

data Tetramino = I | O | T | S | Z | J | L deriving (Ord, Eq, Enum, Bounded, Show)

-- Rotation about the origin
rotRight90 :: (Float, Float) -> (Float, Float)
rotRight90 (y, x) = (x, -y)

rot180 = rotRight90 . rotRight90

rotLeft90 = rotRight90 . rotRight90 . rotRight90

-- Rotate p about p'
rotRight90Point :: (Float, Float) -> (Float, Float) -> (Float, Float)
rotRight90Point (y', x') (y, x) = (\(y, x) -> (y + y', x + x')) $ rotRight90 (y - y', x - x')

rot180Point p' = rotRight90Point p' . rotRight90Point p'
rotLeft90Point p' = rotRight90Point p' . rotRight90Point p' . rotRight90Point p'


basePoints :: Tetramino -> [(Float, Float)]
basePoints t =
  case t of
    I -> [(-1.5, 0.5), (-0.5, 0.5), (0.5, 0.5), (1.5, 0.5)]
    J -> [(-1, 0), (0, 0), (1, 0), (1 , -1)]
    L -> [(-1, 0), (0, 0), (1, 0), (1 , 1)]
    O -> [(0.5, 0.5), (-0.5, 0.5), (0.5, -0.5), (-0.5, -0.5)]
    S -> [(-1, 0), (0, 0), (-1, 1), (0 , -1)]
    T -> [(-1, 0), (0, 0), (0, 1), (0 , -1)]
    Z -> [(-1, 0), (0, 0), (-1, -1), (0 , 1)]

rotatePointsAboutPoint :: Rotation -> (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
rotatePointsAboutPoint rot p ps = map (rotateAboutPoint rot p) ps

addPointPoint :: (Float, Float) -> (Float, Float) -> (Float, Float)
addPointPoint (x, y) (x', y') = (x + x', y + y')

addPointPoints :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
addPointPoints p ps = map (addPointPoint p) ps


rotateAboutPoint North = flip const
rotateAboutPoint South = rot180Point
rotateAboutPoint West  = rotLeft90Point
rotateAboutPoint East  = rotRight90Point

-- blockPoints :: FallingBlock -> [(Int, Int)]
blockPoints (FallingBlock t center rotation) = map (\(x, y) -> (floor x, floor y)) . rotatePointsAboutPoint rotation center . addPointPoints center . basePoints $ t

hasCollided :: Fg -> FallingBlock -> Bool
hasCollided (Fg tetras) = (foldr f k) . blockPoints
  where
    k = False
    f _ True = True
    f (row, col) False
      | row > worldHeight = True
      | otherwise = elem (row, col) (map fst tetras)

wallCollision :: FallingBlock -> Bool
wallCollision = (any f) . blockPoints where
  f :: (Int,Int) -> Bool
  f (y,x) = (x < 0) || (x >= worldWidth)

data Menu = M Int deriving Show
{-
data FallingBlock = FallingBlock { tetra :: Tetramino
                                 , rotation :: Int
                                 , tetShape :: [(Int,Int)] }
-}
