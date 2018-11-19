module Main where

import Lib
import UserInput (eventHandler)
import WorldStepper (worldStepper)
import Renderer (game2Pic)

main :: IO ()
main = play (InWindow "Name at the tope of the window" (1000, 1000) (100, 100)) black 60 (Menu (M 0)) game2Pic eventHandler worldStepper


