module Codeword where
import System.IO.Unsafe


dictionary :: [String]
dictionary = ["abacus"
             ,"boogie"
             ,"bazinga"
             ,"carwash"
             ,"haskell"
             ,"c"
             ,"sycamore"
             ,"water"
             ,"bottle"
             ,"willow"
             ,"stabwound"
             ,"seventeenstitches"
             ,"pen"
             ,"abacus"
             ,"backpack"
             ,"dictionary"
             ,"password"
             ,"bumblebee"
             ,"omega"
             ,"pineapple"
             ,"lambda"
             ,"lynx"
             ,"parsley"
             ,"coriander"
             ,"oregano"
             ,"basil"
             ,"GADTs"]
--dictionary = unsafePerformIO $ lines <$> readFile "src/dictionary.txt"
