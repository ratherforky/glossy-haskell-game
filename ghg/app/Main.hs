module Main where

import Lib
import UserInput (eventHandler)
import WorldStepper (worldStepper)
import Renderer (game2Pic)

main :: IO ()
main = play (InWindow "Fun" (1000,1000) (100,100)) white 60 (initial_game worldWidth worldHeight) game2Pic eventHandler worldStepper
