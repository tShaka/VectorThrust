module VTGraphics where

import Model
import Graphics.UI.GLUT hiding (position, Position)
import FRP.Yampa

-- Sprite represent the graphic of an object, which is given by a set of Positions of Points
type Sprite = [Position]

-- render function to draw current GameState
renderScene :: GameState -> IO ()
renderScene gs = do
    clear [ColorBuffer]
    loadIdentity
    mapM renderGameObject gs
    flush 

-- render function to draw given GameObject
-- third _ is rotation, 7th is size
renderGameObject :: GameObject -> IO ()
renderGameObject (GameObject pos _ _ _ _ _ _ _ _ _ Player) = renderPlayer pos
renderGameObject (GameObject pos _ _ _ _ _ _ _ _ _ Enemy) = renderEnemy pos

-- render function to draw Player object
renderPlayer :: Position -> IO ()
renderPlayer pos = do
    renderPrimitive TriangleFan $ (color $ Color3 (1::GLfloat) (0::GLfloat) (0::GLfloat)) >> (mapM_ (\pos@(Vector x y) -> vertex $ Vertex2 x y) (translateTo playerPointSet pos))

-- render function to draw Enemey object    
renderEnemy :: Position -> IO ()
renderEnemy pos = do
    renderPrimitive TriangleFan $ (color $ Color3 (1::GLfloat) (1::GLfloat) (0::GLfloat)) >> (mapM_ (\pos@(Vector x y) -> vertex $ Vertex2 x y) (translateTo enemyPointSet pos))

-- Sprite representing the Players graphic -> Player is represented by a circle
playerPointSet :: Sprite
playerPointSet = let r=0.15 in map (\t -> Vector (r*cos(t)) (r*sin(t))) [0,0.2..(2*pi)]

-- Sprite representing the Enemies graphic -> Enemy is represented by a circle
enemyPointSet :: Sprite
enemyPointSet = let r=0.1 in map (\t -> Vector (r*cos(t)) (r*sin(t))) [0,0.2..(2*pi)]

-- function for translating a Sprite to a Position
translateTo :: Sprite -> Position -> Sprite
translateTo s pos = map (\pos' -> pos ^+^ pos') s