import Graphics.UI.GLUT
import Data.IORef
import Bindings
 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  -- [adds depth, reduces flicker, makes smooth animation]
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  _window <- createWindow "Super Squares"
  reshapeCallback $= Just reshape
  depthFunc $= Just Less    -- comparison function for depth of the buffer
  angle <- newIORef 0.0
  delta <- newIORef 0.1
  pos <- newIORef (0, 0)
  -- gets input from keyboard
  keyboardMouseCallback $= Just (keyboardMouse delta pos)
  -- updates angle location of each cube
  idleCallback $= Just (idle angle delta)
  displayCallback $= display angle pos
  mainLoop