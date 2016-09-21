module VTGraphics where

import Model
import Graphics.UI.GLUT hiding (position, Position)
import FRP.Yampa
import Control.Monad

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
    renderPlayerShip pos rotation

renderPlayerShip :: Position -> Rotation -> IO ()
renderPlayerShip (Vector pX pY) rotation = do
    forM_ playerTriangleSet $ \x ->
        preservingMatrix $ do
            color $ Color3 (0::GLfloat) (1::GLfloat) (1::GLfloat)
            translate $ Vector3 pX pY 0
            rotate rotation $ Vector3 0 0 1
            
--rotate :: Sprite -> Rotation -> Sprite
--rotate sprite rotation = map (\p -> p ^+^ Vector (sizePlayer * cos rotation) (sizePlayer * sin rotation)) sprite
    
-- render function to draw Enemey object    
renderEnemy :: Position -> IO ()
renderEnemy pos = do
    renderPrimitive TriangleFan $ (color $ Color3 (1::GLfloat) (1::GLfloat) (0::GLfloat)) >> (mapM_ (\pos@(Vector x y) -> vertex $ Vertex2 x y) (translateTo enemyCircleSet pos))

-- Sprite representing the Players graphic -> Player is represented by a circle
playerCircleSet :: Sprite
playerCircleSet = let r=sizePlayer in map (\t -> Vector (r*cos(t)) (r*sin(t))) [0,0.2..(2*pi)]

playerTriangleSet :: Sprite
playerTriangleSet = [Vector 0 0, Vector (-0.13) (-0.18), Vector 0.13 (-0.18)]

-- Sprite representing the Enemies graphic -> Enemy is represented by a circle
enemyCircleSet :: Sprite
enemyCircleSet = let r=sizeEnemy in map (\t -> Vector (r*cos(t)) (r*sin(t))) [0,0.2..(2*pi)]

-- function for translating a Sprite to a Position
translateTo :: Sprite -> Position -> Sprite
translateTo s pos = map (\pos' -> pos ^+^ pos') s