module View where

import Materials

viewAscii :: [[Colour]] -> IO ()
viewAscii colours = putStr $ concatMap ((++ "\n") . concatMap (greyscaleToAscii . getGreyscale)) colours
  where
    greyscaleToAscii 0 = "  "
    greyscaleToAscii l = "00"