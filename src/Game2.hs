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


type Pos = (Float,Float)
type Vel = (Float,Float)
data Action = UP | DOWN | LEFT | RIGHT | SHOOT | NONE
data Type = PLAYER | ENEMY
data GameObject = GameObject Pos Vel Type
type GameState = [GameObject]
type Acc =(Float,Float)

createPlayer :: GameObject
createPlayer = GameObject (0.0,0.0) (0,0) PLAYER
	
createEnemy :: Pos -> GameObject
createEnemy pos = GameObject pos (0,0) ENEMY
	

--refresh :: IORef Action -> GameState -> SF () (GameState)
--refresh actionRef gs = proc () -> 
	
--sf :: sf1 >>> sf2
--sf1 :: SF () (GameState) -> SF () (Pos,Vel)
--sf2 :: SF () (Pos,Vel) -> SF () (GameState)
	
{-
	
accelerate :: IORef Action -> Pos -> Vel -> SF Acc (Pos,Vel)
accelerate actionRef pos vel = do
	action <- readIORef actionRef
	accelerate' action pos vel
-}

{-
accelerate' :: Action -> Pos -> Vel -> SF Acc (Pos,Vel)
accelerate' NONE pos vel = accelerate'' pos vel ((0.0),(0.0))
accelerate' UP pos vel = accelerate'' pos vel ((0.0),(10.0))
accelerate' DOWN pos vel = accelerate'' pos vel ((0.0),(-10.0))
accelerate' LEFT pos vel = accelerate'' pos vel ((-10.0),(0.0))
accelerate' RIGHT pos vel = accelerate'' pos vel ((10.0),(0.0))
-}


--

gameSF :: GameState -> SF Acc GameState
gameSF [(GameObject posS velS PLAYER),(GameObject posA velA ENEMY)] = proc (aX,aY) -> do
	((xS,yS),(vSx,vSy)) <- accelerate posS velS -< (aX,aY)
	((xA,yA),(vAx,vAy)) <- accelerate posA velA -< ((-0.01),(-0.01))
	returnA -< [GameObject (xS,yS) (vSx,vSy) PLAYER, GameObject (xA,yA) (vAx,vAy) ENEMY]


accelerate :: Pos -> Vel -> SF Acc (Pos,Vel)
accelerate (x0,y0) (v0x,v0y) = proc (aX,aY) -> do
	vX <- (v0x+) ^<< integral -< (aX)
	vY <- (v0y+) ^<< integral -< (aY)
	x <- (x0+) ^<< integral -< (vX)
	y <- (y0+) ^<< integral -< (vY)
	returnA -<((x,y),(vX,vY))
	
	
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
	-- saves input
	actionRef <- newIORef NONE
	-- set up ReactHandle
	rh <- reactInit (initr) (actuate) (gameSF initGameState)
	-- set up Callbacks
	displayCallback $= display
	idleCallback $=  Just (idle timeRef actionRef rh)
	reshapeCallback $= Just resizeWindow
	keyboardMouseCallback $= Just (keyboardMouse actionRef)
	
	return ()

	
-- creates initial GameState
initGameState :: GameState
initGameState = [createPlayer, createEnemy (0.7,0.9)]
	

----------------- reactimate functions	 ------------------
-- typed: ReactHandle Input Output -> ReactHandle () (Float,Float)

-- init react
initr :: IO Acc
initr = return (0,0)

--actuate react
actuate :: ReactHandle Acc GameState -> Bool -> GameState -> IO Bool
actuate _ _ gameState = do 
	renderScene gameState
	return False
	

----------------- openGL callbacks ---------------------------

display :: IO ()
display = do
	return ()
	
idle :: IORef UTCTime -> IORef Action -> ReactHandle Acc GameState -> IO ()
idle timeRef inputRef rh = do
	now <- getCurrentTime
	lastTime <- readIORef timeRef
	writeIORef timeRef now
	input <- readIORef inputRef
	let dt = now `diffUTCTime` lastTime
	react rh (realToFrac dt, Just (convAcc input))
	return ()
	
-- helper for converting input to acceleration
convAcc :: Action -> Acc
convAcc UP = (0,1)
convAcc DOWN = (0,(-1))
convAcc LEFT = ((-1),0)
convAcc RIGHT = (1,0)
convAcc NONE = (0,0)
	
resizeWindow :: Size -> IO ()
resizeWindow size = do
	-- TODO keep aspect ratio
	viewport $= (Graphics.UI.GLUT.Position 0 0, size)
	return ()
	
keyboardMouse :: IORef Action -> KeyboardMouseCallback
keyboardMouse inputRef key Down _ _ = case key of
	(SpecialKey KeyLeft) -> inputRef $~! \_ -> LEFT
	(SpecialKey KeyRight) -> inputRef $~! \_ -> RIGHT
	(SpecialKey KeyUp) -> inputRef $~! \_ -> UP
	(SpecialKey KeyDown) -> inputRef $~! \_ -> DOWN
	_ -> return ()
keyboardMouse inputRef key Up _ _ = inputRef $~! \_ -> NONE
keyboardMouse _ _ _ _ _ = return ()
	
	
----------------- physics signal function ---------------------

{-
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
		-}
		
------------------ graphics ------------------------------------

type GLPos = (GLfloat, GLfloat)
type Sprite = [GLPos]

renderScene :: GameState -> IO ()
renderScene [GameObject posS _ PLAYER, GameObject posA _ ENEMY] = do
	clear [ColorBuffer]
	loadIdentity
	--mapM_ renderGameObject gs
	renderPlayer posS
	renderEnemy posA
	flush

{-
---nur zum testen, solange SF nur SF () (Pos,Vel) ist und noch nicht GameState kann
renderTest :: Pos -> IO ()
renderTest pos = do
	clear [ColorBuffer]
	loadIdentity
	renderPlayer pos
	renderEnemy (0.5,0.3)
	renderEnemy ((-0.4),(-0.3))
	renderEnemy (0.8,(-0.2))
	flush
-}	
	
renderGameObject :: GameObject -> IO ()
renderGameObject (GameObject pos _ PLAYER) = renderPlayer pos
renderGameObject (GameObject pos _ ENEMY) = renderEnemy pos

renderPlayer :: Pos -> IO ()
renderPlayer pos = do
	renderPrimitive TriangleFan $ (color $ Color3 (1::GLfloat) (0::GLfloat) (0::GLfloat)) >> (mapM_ (\(x,y) -> vertex $ Vertex2 x y) (translateTo playerPointSet (convertPos pos)))
	
renderEnemy :: Pos -> IO ()
renderEnemy pos = do
	renderPrimitive TriangleFan $ (color $ Color3 (1::GLfloat) (1::GLfloat) (0::GLfloat)) >> (mapM_ (\(x,y) -> vertex $ Vertex2 x y) (translateTo enemyPointSet (convertPos pos)))
	
playerPointSet :: Sprite
playerPointSet = let r=0.15 in map (\t -> (r*cos(t), r*sin(t))) [0,0.2..(2*pi)]

enemyPointSet :: Sprite
enemyPointSet = let r=0.1 in map (\t -> (r*cos(t), r*sin(t))) [0,0.2..(2*pi)]

translateTo :: Sprite -> GLPos -> Sprite
translateTo s (x',y') = map (\(x,y) -> (x+x',y+y')) s

convertPos :: Pos -> GLPos
convertPos (x,y) = (realToFrac x, realToFrac y)


{-
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

-}