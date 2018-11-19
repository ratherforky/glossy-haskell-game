module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Game = Play {for   :: Foreground,
                  back  :: Background,
                  wtf   :: Word2Find,
                  fall  :: FallingBlock}
          | Menu {menu :: Menu}

data Tetramino = I | O | T | S | Z | J | L

data Foreground = Foreground [[Maybe Tetramino]]

data Background = Background [[Maybe (Char, Int)]]

data Word2Find = Word2Find String

data FallingBlock = FallingBlock Tetramino Int [(Int,Int)]

data Menu = M Int

main :: IO ()
main = play FullScreen black 60 (Menu (M 0)) game2Pic eventHandler worldStepper

game2Pic :: Game -> Picture
game2Pic (Menu x _) = Pictures [Color (menuColorByInt x) (Polygon [(-170,-50),(170,-50),(170,50),(-170,50)]),
                              Translate (-140) (-25) (Scale 0.4 0.4  (Text "Play Game")),
                              Color (menuColorByInt (x + 10)) (Line [(-170,-50),(170,-50),(170,50),(-170,50),(-170,-50)])
                             ]
game2Pic (Play a) = undefined

eventHandler :: Event -> Game -> Game
eventHandler e g = g

worldStepper :: Float -> Game -> Game
worldStepper f g = g
