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
renderGameObject (GameObject pos _ _ rotation _ _ _ _ _ _ Player) = renderPlayer pos rotation
renderGameObject (GameObject pos _ _ rotation _ _ _ _ _ _ Enemy) = renderEnemy pos

-- render function to draw Player object
renderPlayer :: Position -> Rotation -> IO ()
renderPlayer pos rotation = do
    renderPrimitive TriangleFan $ (color $ Color3 (1::GLfloat) (0::GLfloat) (0::GLfloat)) >> (mapM_ (\pos@(Vector x y) -> vertex $ Vertex2 x y) (translateTo playerCircleSet pos))
    renderPrimitive TriangleFan $ (color $ Color3 (0::GLfloat) (1::GLfloat) (1::GLfloat)) >> (mapM_ (\pos@(Vector x y) -> vertex $ Vertex2 x y) (translateTo playerTriangleSet posTriangle))
        where
            posTriangle = pos ^+^ 0.15 *^ Vector (cos rotation) (sin rotation)
    
-- render function to draw Enemey object    
renderEnemy :: Position -> IO ()
renderEnemy pos = do
    renderPrimitive TriangleFan $ (color $ Color3 (1::GLfloat) (1::GLfloat) (0::GLfloat)) >> (mapM_ (\pos@(Vector x y) -> vertex $ Vertex2 x y) (translateTo enemyCircleSet pos))

-- Sprite representing the Players graphic -> Player is represented by a circle
playerCircleSet :: Sprite
playerCircleSet = let r=0.15 in map (\t -> Vector (r*cos(t)) (r*sin(t))) [0,0.2..(2*pi)]

playerTriangleSet :: Sprite
playerTriangleSet = [Vector 0 0, Vector (-0.15) (-0.15), Vector 0.15 (-0.15)]

-- Sprite representing the Enemies graphic -> Enemy is represented by a circle
enemyCircleSet :: Sprite
enemyCircleSet = let r=0.1 in map (\t -> Vector (r*cos(t)) (r*sin(t))) [0,0.2..(2*pi)]

-- function for translating a Sprite to a Position
translateTo :: Sprite -> Position -> Sprite
translateTo s pos = map (\pos' -> pos ^+^ pos') s