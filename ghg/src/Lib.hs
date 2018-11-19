module Lib 
  ( module Lib
  , module Graphics.Gloss
  , module Graphics.Gloss.Interface.Pure.Game
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Game = Play {for   :: Foreground,
                  back  :: Background,
                  wtf   :: Word2Find,
                  fall  :: FallingBlock,
                  accTime :: Float}
          | Menu {menu :: Menu}

data Tetramino = I | O | T | S | Z | J | L

data Foreground = Foreground [[Maybe Tetramino]]

data Background = Background [[Maybe (Char, Int)]]

data Word2Find = Word2Find String

data FallingBlock = FallingBlock { tetra :: Tetramino
                                 , rotation :: Int
                                 , tetShape :: [(Int,Int)] }

data Menu = M Int
