module Display (display) where

import Graphics.UI.GLUT
import Control.Monad
import Cube
import Points
 
display :: DisplayCallback
display = do
    scale 0.7 0.7 (0.7 :: GLfloat)
    clear [ColorBuffer]
    forM_ (points 7) $ \(x,y,z) ->
         preservingMatrix $ do
        -- normalizes color from [-1,1] to [0,1] for openGL
        color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
        translate $ Vector3 x y z
        cubez 0.1
    flush