module Main where

import Lib
import UserInput (eventHandler)
import WorldStepper (worldStepper)
import Renderer (game2Pic)

main :: IO ()
main = do
  gen <- newStdGen
  play (InWindow "Fun" (1000,1000) (100,100)) white 60 (initial_game (randomVals gen)) game2Pic eventHandler worldStepper

randomVals :: StdGen -> [Float]
randomVals seed = randomRs (-100,100) seed
