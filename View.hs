module View where

import qualified Debug.Trace as Debug
import Materials

viewAscii :: [[Colour]] -> IO ()
viewAscii = putStr . asciiString

asciiString :: (Foldable t1, Foldable t2) => t1 (t2 Colour) -> [Char]
asciiString colours = reverse $ concatMap ( ('\n' : ) . reverse . concatMap (greyscaleToAscii . getGreyscale)) colours
  where
    greyscaleToAscii val
      | val < 0.1 = " "
      | val < 0.31 = "."
      | val < 0.5 = "-"
      | val < 0.7 = ":"
      | val < 0.9 = "+"
      | otherwise = "#"