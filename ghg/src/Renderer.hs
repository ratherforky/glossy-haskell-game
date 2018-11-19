module Renderer (game2Pic) where

import Lib
    
game2Pic :: Game -> Picture
game2Pic (Menu x) = Blank
game2Pic (Play{}) = Blank