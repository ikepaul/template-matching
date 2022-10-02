module Draw where

import Graphics.UI.Threepenny

canvasWidth :: Double
canvasWidth = 300
canvasHeight :: Double
canvasHeight = 300 

draw :: Element -> UI ()
draw canvas = do
  pure canvas # set fillStyle (solidColor (RGB 0 0 0))
  fillCanvas canvas

fillCanvas :: Element -> UI ()
fillCanvas canvas = do 
  pure canvas # set fillStyle (solidColor (RGB 0 0 0))
  fillRect (0, 0) canvasWidth canvasHeight canvas

drawCircle :: Element -> Point -> UI ()
drawCircle canvas (x, y) = do
  beginPath canvas
  arc (x, y) 20 0 (2 * pi) canvas
  fill canvas
