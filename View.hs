module View where

import Materials
import qualified Debug.Trace as Debug

viewAscii :: [[Colour]] -> IO ()
viewAscii colours = putStr $ concatMap ((++ "\n") . concatMap (greyscaleToAscii . getGreyscale)) colours
  where
    greyscaleToAscii val
      | val < 0.1 = " "
      | val < 0.31 = "."
      | val < 0.5 = "-"
      | val < 0.7 = ":"
      | val < 0.9 = "+"
      | otherwise  = "#"