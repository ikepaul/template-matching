module Main where

import Draw
import Graphics.UI.Threepenny
import Data.IORef
import Shape
import Templates

main :: IO ()
main = startGUI defaultConfig initGame

initGame :: Window -> UI ()
initGame window = do
  pure window # set title "testing"

  canvas <- canvas # set height (round canvasHeight) # set width (round canvasWidth)

  mouseActivationRef <- liftIO $ newIORef (False :: Bool)
  myShapeRef <- liftIO $ newIORef ([] :: Shape)
  --myShapesRef <- liftIO $ newIORef ([] :: [Shape])
  body <- getBody window
  pure body
    #+ [column [element canvas]]
  
  on keydown body $ \(keyCode) -> do
    case keyCode of
      67 -> do
        liftIO $ writeIORef myShapeRef []
        fillCanvas canvas
      68 -> do
        liftIO $ print (length myTemplates)
        return ()
        --currentShapes <- liftIO $ readIORef myShapesRef
        --liftIO $ writeFile "./templates.txt" (show currentShapes)
      78 -> do
        currentShape <- liftIO $ readIORef myShapeRef
        --liftIO $ modifyIORef myShapesRef (currentShape :)
        liftIO $ writeIORef myShapeRef []
        fillCanvas canvas
      82 -> do
        return ()
        --liftIO $ modifyIORef myShapesRef (tail)
      otherwise -> return ()


  on mousedown canvas $ \(x, y) -> do
    liftIO $ writeIORef mouseActivationRef True
    liftIO $ modifyIORef myShapeRef ((x,y) :)
    pure canvas # set fillStyle (solidColor (RGB 255 255 255))
    drawCircle canvas (x, y)
  
  on mouseup canvas $ \(x, y) -> do
    liftIO $ writeIORef mouseActivationRef False

  on mousemove canvas $ \(x,y) -> do
    isPressed <- liftIO $ readIORef mouseActivationRef
    if (isPressed) 
      then do
        drawCircle canvas (x,y)
        liftIO $ modifyIORef myShapeRef ((x,y) :)
      else return ()

  body <- getBody window
  pure body
    #+ [column [element canvas]]

  timer1 <- set interval 3000 timer
  start timer1

  on tick timer1 $ \() -> gameLoop window canvas
  --gameLoop window canvas
  fillCanvas canvas
  return ()

gameLoop :: Window -> Element -> UI ()
gameLoop window canvas = do
  --pure canvas # set fillStyle (solidColor (RGB 100 100 100))
  --drawCircle canvas (400, 400)
  return ()
