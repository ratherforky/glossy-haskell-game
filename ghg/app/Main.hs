module Main where

import Lib
import UserInput (eventHandler)
import WorldStepper (worldStepper)
import Renderer (game2Pic)

main :: IO ()
main = play FullScreen black 60 (Menu (M 0)) game2Pic eventHandler worldStepper


