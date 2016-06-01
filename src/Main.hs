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
	-- saves position of player
	playerPos <- newIORef (realToFrac 0, realToFrac 0)
	-- set up ReactHandle
	rh <- reactInit (initr) (actuate playerPos) (bouncingBall 5)
	-- set up Callbacks
	displayCallback $= display
	idleCallback $=  Just (idle timeRef rh)
	reshapeCallback $= Just resizeWindow
	keyboardMouseCallback $= Just (keyboardMouse playerPos)
	
	return ()


----------------- reactimate functions	 ------------------
-- typed: ReactHandle Input Output -> ReactHandle () (Float,Float)

-- init react
initr :: IO ()
initr = return ()

--actuate react
actuate :: IORef (GLfloat,GLfloat) -> ReactHandle () (Float, Float) -> Bool -> (Float, Float) -> IO Bool
actuate playerPos _ _ (pos,vel) = do
	renderScene playerPos pos
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
	
resizeWindow :: Size -> IO ()
resizeWindow size = do
	-- TODO keep aspect ratio
	viewport $= (Graphics.UI.GLUT.Position 0 0, size)
	return ()
	
keyboardMouse :: IORef(GLfloat,GLfloat) -> KeyboardMouseCallback
keyboardMouse playerPos key Down _ _ = case key of
	(SpecialKey KeyLeft) -> playerPos $~! \(x,y) -> (x-0.1,y)
	(SpecialKey KeyRight) -> playerPos $~! \(x,y) -> (x+0.1,y)
	(SpecialKey KeyUp) -> playerPos $~! \(x,y) -> (x,y+0.1)
	(SpecialKey KeyDown) -> playerPos $~! \(x,y) -> (x,y-0.1)
	_ -> return ()
keyboardMouse _ _ _ _ _ = return ()
	
	
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

-- used to change the position of moveable circle
translateTo' :: Circle -> (GLfloat,GLfloat) -> Circle
translateTo' c (x,y) = map (\(x',y') -> (x'+x, y'+y)) c

-- converts "normal" haskell variables into OpenGL variables
convertToGL :: Model.Position -> (GLfloat, GLfloat)
convertToGL (Model.Position x y) = (realToFrac x, realToFrac y)
	
-- draw moveable circle (=player)
renderPlayer :: IORef (GLfloat,GLfloat) -> IO ()
renderPlayer posRef = do
		(x,y) <- readIORef posRef
		repositionCamera (x,y)
		renderPrimitive TriangleFan $ (color $ Color3 (1 :: GLfloat) (1 :: GLfloat) (1 :: GLfloat)) >> (mapM_ (\(x,y) -> vertex $ Vertex2 x y) (translateTo' player (x,y)))
	
-- draw the ball
renderBall :: Float -> IO ()
renderBall posY = do
	-- draw not moving circle
	renderPrimitive TriangleFan $ (color $ Color3 (1 :: GLfloat) (1 :: GLfloat) (0 :: GLfloat)) >> (mapM_ (\(x,y) -> vertex $ Vertex2 x y) (translateTo player (Model.Position (-0.5) 0)))
	--draw falling ball
	renderPrimitive TriangleFan $ (color $ Color3 (1 :: GLfloat) (1 :: GLfloat) (0 :: GLfloat)) >> (mapM_ (\(x,y) -> vertex $ Vertex2 x y) (translateTo player (Model.Position 0 posY)))
	
-- draw scene
renderScene :: IORef (GLfloat, GLfloat) -> Float -> IO ()
renderScene playerPos mBallPos = do
	clear [ColorBuffer]
	loadIdentity
	renderPlayer playerPos
	renderBall mBallPos
	flush
	
-- position camera based on key commands
repositionCamera :: (GLfloat, GLfloat) -> IO ()
repositionCamera (x,y) = do
	let camPos = Vertex3 (realToFrac x) (realToFrac y) (1::GLdouble)
	let camView = Vertex3 (realToFrac x) (realToFrac y) (0::GLdouble)
	let upVec = Vector3 (0::GLdouble) (1::GLdouble) (0::GLdouble)
	lookAt camPos camView upVec

