-- reactimate loop funktioniert nicht zusammn mit GLUT --
-- daher muss reactInit + react verwendet werden --
-- Grund: reactimate stellt mainLoop dar -> mainLoop muss in GLUT aber der OpenGL loop sein --
-- Quelle: https://hackage.haskell.org/package/Yampa-0.10.5/docs/FRP-Yampa.html#t:https://hackage.haskell.org/package/Yampa-0.10.5/docs/FRP-Yampa.html#v:reactimate --

---------------------------------------------------------------------------

-- Code nach Quelle: http://code.haskell.org/frag/src/Main.hs --

---------------------------------------------------------------------------

{-# LANGUAGE Arrows #-}

import Model
import Graphics.UI.GLUT
import Data.IORef
import FRP.Yampa
import FRP.Yampa.Utilities
import Control.Arrow
import Data.Time.Clock


-- set up OpenGL and start program
main :: IO ()
main = do
	(_progname, _args) <- getArgsAndInitialize
	_window <- createAWindow "OpenGL Test"
	mainLoop
	
-- create window with specified attributes and set up game
createAWindow :: String -> IO ()
createAWindow title = do
	-- set up and create window
	initialWindowSize $= (Size 640 480)
	createWindow title
	-- set up IORefs
	t <- getCurrentTime
	timeRef <- newIORef t
	-- set up ReactHandle
	rh <- reactInit (initr) (actuate) (bouncingBall 5)
	-- set up Callbacks
	displayCallback $= display
	idleCallback $=  Just (idle timeRef rh)
	
	return ()


----------------- reactimate functions	 ------------------
-- typed: ReactHandle Input Output -> ReactHandle () (Float,Float)

-- init react
initr :: IO ()
initr = return ()

--actuate react
actuate :: ReactHandle () (Float, Float) -> Bool -> (Float, Float) -> IO Bool
actuate _ _ (pos,vel) = do
	renderBall pos
	return False
	

----------------- openGL callbacks ---------------------------

display :: IO ()
display = do
	return ()
	
idle :: IORef UTCTime -> ReactHandle () (Float,Float) -> IO ()
idle timeRef rh = do
	now <- getCurrentTime
	lastTime <- readIORef timeRef
	writeIORef timeRef now
	let dt = now `diffUTCTime` lastTime
	react rh (realToFrac dt, Just ())
	return ()
	
	
----------------- physics signal function ---------------------

type Pos = Float
type Vel = Float

-- free falling ball --
fallingBall :: Pos -> Vel -> SF () (Pos, Vel)
fallingBall y0 v0 = proc () -> do
	v <- (v0+) ^<< integral -< (-9.81)
	y <- (y0+) ^<< integral -< v
	returnA -< (y,v)

-- detecting when ball goes through the floor --
fallingBall' :: Pos -> Vel -> SF () ((Pos,Vel), Event (Pos,Vel))
fallingBall' y0 v0 = proc () -> do
	yv@(y,_) <- fallingBall y0 v0 -< ()
	hit <- edge -< y <= 0
	returnA -< (yv, hit `tag` yv)
	
-- making the ball bounce --
bouncingBall :: Pos -> SF () (Pos, Vel)
bouncingBall y0 = bbAux y0 0.0
	where
		bbAux y0 v0 = switch (fallingBall' y0 v0) $ \(y,v) -> bbAux y (-v)
		
		
------------------ graphics ------------------------------------

-- represent a Position with OpenGL variables
type GLPosition = (GLfloat, GLfloat)
-- represent a circle by a list of positions of point which form connected a circle
type Circle = [GLPosition]

-- represent a circle by a set of points, their connection creates circle
createCircle :: GLfloat -> Circle
createCircle r = map (\t -> (r*cos (t), r*sin(t))) [0,0.2..(2*pi)]

-- represent player as circle
player :: Circle
player = createCircle 0.2

-- used to change the position of a Circle
translateTo :: Circle -> Model.Position -> Circle
translateTo c pos = let (x'', y'') = convertToGL pos in map (\(x',y') -> (x'+x'', y'+y'')) c

-- converts "normal" haskell variables into OpenGL variables
convertToGL :: Model.Position -> (GLfloat, GLfloat)
convertToGL (Model.Position x y) = (realToFrac x, realToFrac y)

-- draw the ball
renderBall :: Float -> IO ()
renderBall posY = do
	clear [ColorBuffer]
	renderPrimitive TriangleFan $ (color $ Color3 (1 :: GLfloat) (1 :: GLfloat) (0 :: GLfloat)) >> (mapM_ (\(x,y) -> vertex $ Vertex2 x y) (translateTo player (Model.Position 0 posY)))
	flush
	


