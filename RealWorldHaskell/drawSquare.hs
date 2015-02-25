import Graphics.UI.GLUT
import Control.Monad
import Cube

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop
 
color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g (b :: GLfloat)

vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO()
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  renderPrimitive Quads $ do
      let squares = map centeredSquare [2.0,1.9.. 0.1] 
      sequence_ (zipWith ($) squares (take 20 $ cycle [(0.6,0.4,0),(1,0.5,0),(0,0.5,1)]))
  flush

centeredSquare :: GLfloat -> (GLfloat,GLfloat,GLfloat) -> IO ()
centeredSquare sideLen color = do
    let side = sideLen / 2
        (r,g,b) = color
    color3f r g b
    vertex3f side (-side) 0     -- right bottom vertex
    vertex3f side side 0        -- right top vertex
    vertex3f (-side) side 0     -- Left top 
    vertex3f (-side) (-side) 0  -- left bottom
