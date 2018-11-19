module Lib
  ( module Lib
  , module Graphics.Gloss
  , module Graphics.Gloss.Interface.Pure.Game
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

data Game = Play {for   :: Foreground,
                  back  :: Background,
                  wtf   :: Word2Find,
                  fall  :: FallingBlock,
                  word  :: String,
                  rands :: [Float]}
          | Menu {menu :: Menu}
          deriving Show

-- initial_game width height = Play
--   where
--     for  = Foreground ((map . map) (const Nothing) [[1..width] | _ <- [1..height]])
--     back = Background ((map . map) (const Nothing) [[1..width] | _ <- [1..height]])
--     wtf  = "hello"
--     fall = FallingBlock I [(10, 10), (10, 11), (10, 12), (10, 13)]
--     word = ""

data Tetramino = I | O | T | S | Z | J | L deriving Show

data Background = Background [[(Maybe Char, Int)]] deriving Show

data Foreground = Foreground [[Maybe Tetramino]] deriving Show

randomVals :: Float -> [Float]
randomVals seed = randomRs (-100,100) (mkStdGen ((fromInteger . toInteger . floor) seed))

blockPoints :: FallingBlock -> [(Int,Int)]
blockPoints _ = []

initData :: Game
--initData = Menu {menu = (M 0)}
initData = (Play {for = initArrFor, back = initArrBack, wtf = (Word2Find "Codeword"), fall = (FallingBlock I (0,0) North), word = "Hi"})

initArrFor :: Foreground
initArrFor = Foreground ([b,b,b,b,b,b,b,b]) where
  b = [(Just O),(Just T),(Just S),(Just Z)]

initArrBack :: Background
initArrBack = Background ([b,b,b,b,b,b,b,b]) where
  b = [(Nothing,0),(Nothing,0),(Nothing,0),(Nothing,0)]

data Word2Find = Word2Find String deriving Show

data FallingBlock = FallingBlock Tetramino (Float, Float) Rotation deriving Show

data Rotation = North | South | East | West deriving Show


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

-- blockPoints :: FallingBlock -> [(Int, Int)]
-- blockPoints (FallingBlock tetramino center rotation) =
  -- case tetramino of
    -- I ->

data Menu = M Int deriving Show
