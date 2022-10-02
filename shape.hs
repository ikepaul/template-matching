module Shape where

import Graphics.UI.Threepenny hiding(map, div)
import Draw

type Shape = [Point]

hausdorrf :: Point -> Shape -> Double
hausdorrf (px, py) = minimum . map (\(spx, spy) -> sqrt ((px - spx)^2  + (py - spy)^2))

tanimoto :: Shape -> Shape -> Double
tanimoto s1 s2 = snd result / fst result where
  result = count $ n ++ m
  count = foldl (\b a -> (fst b + 1, snd b + (if a then 1 else 0))) (0,0)
  n = overlapping s1 s2
  m = overlapping s2 s1
  overlapping a b = map (\sp -> 0.1*canvasWidth > hausdorrf sp b) a 


threshHold = 0.1