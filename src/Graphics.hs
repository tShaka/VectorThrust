import Graphics.UI.GLUT
import Model

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

-- represent enemy
enemy :: Circle
enemy = createCircle 0.1



-- OpenGL main loop
main :: IO ()
main = do
	(_progName, _args) <- getArgsAndInitialize
	_window <- createWindow "OpenGL Test"
	displayCallback $= display
	mainLoop
	
-- OpenGL display: draws all Objects of the Game	
display :: DisplayCallback
display = do
	clear [ColorBuffer]
	drawPlayer (Model.Position 0.0 0.0)
	drawEnemy (Model.Position 0.5 0.3)
	flush
	
	

-- draws all Objects in its given list (main drawing function)
drawObjects :: [Model.Object] -> IO ()
drawObjects objs = mapM_ drawObject objs	
	
-- draw one Object (helper function for drawObjects)
drawObject :: Model.Object -> IO ()
drawObject (Object (MassPoint pos _) Player) = drawPlayer pos
drawObject (Object (MassPoint pos _) Enemy) = drawEnemy pos

-- draws the Player (helper function for drawObject)
drawPlayer :: Model.Position -> IO ()
drawPlayer pos = renderPrimitive TriangleFan $ (color $ Color3 (1 :: GLfloat) (0 :: GLfloat) (1 :: GLfloat)) >> (mapM_ (\(x,y) -> vertex $ Vertex2 x y) (translateTo player pos))
	
-- draws an Enemy (helper function for drawObject)
drawEnemy :: Model.Position -> IO ()
drawEnemy pos = renderPrimitive TriangleFan $ (color $ Color3 (0 :: GLfloat) (1 :: GLfloat) (1 :: GLfloat)) >> (mapM_ (\(x,y) -> vertex $ Vertex2 x y) (translateTo enemy pos))

-- used to change the position of a Circle
translateTo :: Circle -> Model.Position -> Circle
translateTo c pos = let (x'', y'') = convertToGL pos in map (\(x',y') -> (x'+x'', y'+y'')) c

-- converts "normal" haskell variables into OpenGL variables
convertToGL :: Model.Position -> (GLfloat, GLfloat)
convertToGL (Model.Position x y) = (realToFrac x, realToFrac y)

