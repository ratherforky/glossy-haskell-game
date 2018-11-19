module Main where

import Lib

data Game = Play {for   :: Foreground,
                  back  :: Background,
                  wtf   :: Word2Find,
                  fall  :: FallingBlock}
          | Menu {menu :: Menu}

data Tetramino = L | I | T | S | Z | B

data Foreground = Foreground [[Maybe Tetramino]]

data Background = Background [[Maybe (Char, Int)]]

data Word2Find = Word2Find String

data FallingBlock = FallingBlock Tetramino Int [(Int,Int)]

data Menu = M Int

main :: IO ()
main = play FullScreen black 60 (Menu (M 0)) game2Pic eventHandler worldStepper

game2Pic :: Game -> Picture
game2Pic = undefined

eventHandler :: Event -> Game -> Game
eventHandler = undefined

worldStepper :: Float -> Game -> Game
worldStepper = undefined
