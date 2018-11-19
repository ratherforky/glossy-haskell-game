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

data Tetramino = I | O | T | S | Z | J | L

data Foreground = Foreground [[Maybe Tetramino]]

data Background = Background [[(Maybe Char, Int)]]

data Word2Find = Word2Find String

data FallingBlock = FallingBlock Tetramino (Float,Float) Rotation

data Rotation = North | South | East | West

data Menu = M Int

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
