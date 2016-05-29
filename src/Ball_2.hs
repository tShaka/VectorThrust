{-# LANGUAGE Arrows #-}

import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.Time.Clock
import FRP.Yampa
import FRP.Yampa.Vector3
import FRP.Yampa.Utilities
import Unsafe.Coerce

import Graphics.UI.GLUT hiding (Level,Vector3(..),normalize)
import qualified Graphics.UI.GLUT as G(Vector3(..))
 

 
type Pos = Double
type Vel = Double
type R = GLdouble

movingBall :: Pos -> SF() (Pos,Vel) 
movingBall y0 =  (constant (-1.81) 
                  >>> integral) >>> ((integral >>^ (+ y0)) &&& identity)

main :: IO ()
--Main Function    Init, input, output, called function
main = reactimate (initOGL) 
                  (\ _ -> threadDelay 200000 >> return(0.1, Nothing))
                  (\ _ (pos, vel) -> putStrLn ("pos: " ++ show pos ++ ", vel: " ++ show vel)
                  >> draw pos >> return False)
                  (movingBall 10.0)


--Visuals:

initOGL :: IO ()
initOGL = do
    getArgsAndInitialize
    initialDisplayMode $= [ WithDepthBuffer, DoubleBuffered ]
    createWindow "Bounce"
    depthFunc          $= Just Less
    clearColor         $= Color4 0 0 0 0
    light (Light 0)    $= Enabled
    lighting           $= Enabled
    lightModelAmbient  $= Color4 0.5 0.5 0.5 1
    diffuse (Light 0)  $= Color4 1 1 1 1
    blend              $= Enabled
    blendFunc          $= (SrcAlpha, OneMinusSrcAlpha)
    colorMaterial      $= Just (FrontAndBack, AmbientAndDiffuse)
    reshapeCallback    $= Just resizeScene
    return ()
    
-- Copied from Cuboid (which copied from reactive-glut)
resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1) -- prevent divide by zero
resizeScene s@(Size width height) = do
  -- putStrLn "resizeScene"
  viewport   $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (w2/h2) 1 1000
  matrixMode $= Modelview 0
 where
   w2 = half width
   h2 = half height
   half z = realToFrac z / 2
   

--Render from Cuboid, mildly modified

--draw' :: SF GameState (IO ())
--draw' = arr $ (\gs -> do
--        
--        renderGame gs
--        swapBuffers)

draw :: Pos -> IO ()
draw pos = do
    clear [ ColorBuffer, DepthBuffer ]
    loadIdentity
    --translate $ G.Vector3 (0 :: R) 0 (-2*(fromInteger $ size l))
    -- TODO: calculate rotation axis based on rotX/Y
    --rotate (rotX * 10) xAxis
    --color $ Color3 (1 :: R) 1 1
    --position (Light 0) $= Vertex4 0 0 0 1
    --renderObject Wireframe (Cube $ fromInteger $ size l)
    renderPlayer $ vector3 2 (unsafeCoerce pos) 2
    --renderGoal (p3DtoV3 $ endPoint l)
    --mapM_ (renderObstacle . p3DtoV3) $ obstacles l
    where size2 :: R
          size2 = (fromInteger $ 6)/2
          green  = Color4 0.8 1.0 0.7 0.9 :: Color4 R
          greenG = Color4 0.8 1.0 0.7 1.0 :: Color4 R
          red    = Color4 1.0 0.7 0.8 1.0 :: Color4 R
          renderShapeAt s p = preservingMatrix $ do
            translate $ G.Vector3 (0.5 - size2 + vector3X p)
                                  (0.5 - size2 + vector3Y p)
                                  (0.5 - size2 + vector3Z p)
            renderObject Solid s
          renderObstacle = (color green >>) . (renderShapeAt $ Cube 1)
          renderPlayer   = (color red >>) . (renderShapeAt $ Sphere' 0.5 20 20)
          renderGoal     =
            (color greenG >>) . (renderShapeAt $ Sphere' 0.5 20 20)
            

