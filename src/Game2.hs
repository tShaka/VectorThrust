-- reactimate loop funktioniert nicht zusammn mit GLUT --
-- daher muss reactInit + react verwendet werden --
-- Grund: reactimate stellt mainLoop dar -> mainLoop muss in GLUT aber der OpenGL loop sein --
-- Quelle: https://hackage.haskell.org/package/Yampa-0.10.5/docs/FRP-Yampa.html#t:https://hackage.haskell.org/package/Yampa-0.10.5/docs/FRP-Yampa.html#v:reactimate --

---------------------------------------------------------------------------

-- Code nach Quelle: http://code.haskell.org/frag/src/Main.hs --

---------------------------------------------------------------------------

{-# LANGUAGE Arrows #-}

import Model
import Graphics.UI.GLUT hiding (Position)
import Data.IORef
import FRP.Yampa
import FRP.Yampa.Utilities
import Control.Arrow
import Data.Time.Clock
import Data.Maybe


createPlayer :: GameObject
createPlayer = GameObject zeroVector zeroVector zeroVector Player
    
createEnemy :: Position -> GameObject
createEnemy pos = GameObject pos zeroVector zeroVector Enemy
    

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

-- TODO Rotation
-- Variablen: Accel-Value acc, Rationswinkel rot (0 ist in diesem Beispiel oben)
-- Formel y: acc * cos(rot*pi() / 180)
-- Formel x: acc * sin(rot*pi() / 180)
-- Roation sollte bei Druck auf KeyLeft / KeyRight einfach den Rotationswert modifizieren

-- TODO : Collision
--proc acc -> do
--     rec
--     (xs,vs) <- ship      -< (acc, dvs)
--     (xa,va) <- asteroid  -< (acc, dva)
--     let (dxs, dvs, dxa, dva) = coll (xs, vs, xa, va)
--      (x's, v's, x'a, v'a) <- sum? -< (dxs, dvs, dxa, dva)
--      returnA -< (x's, x'a)


collision :: (Position,Velocity,Position,Velocity) -> (Event Position, Event Velocity, Event Position, Event Velocity)
-- Ruft die Kollisionsfunktionen auf, die wir nachher auslagern werden
collision (ps,vs,pa,va) = 
    if isColliding (ps,pa) then afterCollision (ps,vs,pa,va) else if collidesWithWall (ps,pa) then afterWallCollision (ps,vs,pa,va) else (NoEvent, NoEvent, NoEvent, NoEvent) --(NoEvent,NoEvent,NoEvent,NoEvent)

collidesWithWall :: (Position, Position) -> Bool
collidesWithWall (Vector x y, Vector x' y')
    | x < (-1) = True
    | y < (-1) = True
    | x > 1 = True
    | y > 1 = True
    | x' < (-1) = True
    | y' < (-1) = True
    | x' > 1 = True
    | y' > 1 = True
    | otherwise = False
    
collidesWithWall' :: SF (Position,Velocity) (Event Velocity)
collidesWithWall' = proc (Vector x y, vel) -> do
    hit <- edge -< x < (-1) || x > 1 || y < (-1) || y > 1
    returnA -< hit `tag` ((-1) *^ vel)
    
    
afterWallCollision :: (Position, Velocity, Position, Velocity) -> (Event Position, Event Velocity, Event Position, Event Velocity)
afterWallCollision (ps,vs,pa,va) = (dpos1, dv1, dpos2, dv2)
    where
        dpos1 = Event $ Vector (-0.01) (-0.01)
        dv1 = Event $(-1) *^ vs
        dpos2 = Event $ Vector (-0.01) (-0.01)
        dv2 = Event $ (-10) *^ va
    
afterCollision :: (Position,Velocity,Position,Velocity) -> (Event Position, Event Velocity, Event Position, Event Velocity)
--Dreht bei einer Kollision einfach die involvierten Geschwindigkeiten um. 
--Wird später basierend auf Objektmasse und Impulsübertragung funktionieren.
afterCollision (ps,vs,pa,va) = (dpos1,dv1,dpos2,dv2)
    where
        dpos1 = Event $ Vector (-1.01) (-1.01) --(Vector (-0.01) (-0.01)) ^+^ ps
        dv1 = Event $(-2) *^ vs
        dpos2 = Event $ Vector (-1.01) (-1.01) --(Vector (-0.01) (-0.01)) ^+^ pa
        dv2 = Event $ (-10) *^ va
    
--Überprüft die Kollision von zwei Objekten. Aktuell haben wir nur 2.
-- TODO: Andere Objekte als Kreise
-- TODO: distance durch norm ersetzen    
isColliding :: (Position,Position) -> Bool
isColliding (posS,posA) = ((distance dpos) - (rs + ra)) <= sigma
    where
        rs = 0.15 :: GLfloat
        ra = 0.10 :: GLfloat
        dpos = posS ^-^ posA
        sigma = 0
--TODO: Collision Angle:
-- distance :: Vector -> GLfloat
-- ColAng = arcsin ( y/distance)*180 / pi()
        
distance :: Vector -> GLfloat
distance (Vector x y) = sqrt(x*x + y*y)
        
        {-}
gameSF :: GameState -> SF Acceleration GameState
gameSF [(GameObject posS velS accS Player),(GameObject posA velA accA Enemy)] = proc accShip -> do
    rec
     (posS,velS) <- accelerate posS velS -< accShip
     (posA,velA) <- accelerate posA velA -< Vector (-0.01) (-0.01)
     let (dps, dvs, dpa, dva) = collision (posS, velS, posA, velA)
     --(ps', vs', pa', va') <- accumHoldBy (^+^) zeroVector -< Event (dps, dvs, dpa, dva) -- ersetzt mit den 4 unteren zeilen
     ps' <- accumHoldBy (^+^) zeroVector -< Event dps
     vs' <- accumHoldBy (^+^) zeroVector -< Event dvs
     pa' <- accumHoldBy (^+^) zeroVector -< Event dpa
     va' <- accumHoldBy (^+^) zeroVector -< Event dva
    --returnA -< [GameObject ps' vs' accS Player, GameObject pa' va' accA Enemy] -- ersetzt mit unterer zeile
    returnA -< [GameObject (ps' ^+^ posS) (vs' ^+^ velS) accS Player, GameObject (pa' ^+^ posA) (va' ^+^ velA) accA Enemy]
-}

--Neue Kollisionsfunktion
--TODO: Geschwindigkeitsberechnung
gameSF :: GameState -> SF Acceleration GameState
gameSF [(GameObject posS velS accS Player),(GameObject posA velA accA Enemy)] = proc accShip -> do
    rec
        -- movement + collision player
        velPreS <- iPre velS -< vS
        colS <- collisionSF -< (posS', velPreS, posA', velPreA)
        vS <- (velS ^+^) ^<< impulseIntegral -< (accShip, colS)
        posS' <- (posS ^+^) ^<< integral -< vS
        -- movement + collision enemy
        velPreA <- iPre velA -< vA
        colA <- collisionSF -< (posA', velPreA, posS', velPreS)
        vA <- (velA ^+^) ^<< impulseIntegral -< (Vector (-0.01) (-0.01), colA)
        posA' <- (posA ^+^) ^<< integral -< vA
        -- return new GameState
    returnA -< [GameObject posS' vS accS Player, GameObject posA'  vA accA Enemy]
    {-
    rec
        dvs <- collidesWithWall' -< (posS,accShip)
        vs <- (velS ^+^) ^<< impulseIntegral -< (accShip, dvs)
        --vs <- (velS ^+^) ^<< impulseIntegral -< (accShip, das)
        ps <- (posS ^+^) ^<< integral -< vs
        va <- (velA ^+^) ^<< impulseIntegral -< (Vector (-0.01) (-0.01), daa)
        pa <- (posA ^+^) ^<< integral -< va
    returnA -< [GameObject ps vs accS Player, GameObject pa va accA Enemy]
        where
            (dps, das, dpa, daa) = collision (posS,velS,posA,velA)
-}

{- TODO: Hier müssen wir die korrekte neue Geschwindigkeit berechnen. 
Dafür müssen wir Rotation, GameObjectMass, und GameObjectElas übergenen; 
Die Größe wird bereits für die Kollisionserkennung verwendet
-}
collisionSF :: SF (Position,Velocity,Position,Velocity) (Event Velocity)
collisionSF = proc (p1,v1,p2,v2) -> do
    hit <- edge -< isColliding (p1,p2) || detectWall p1
    returnA -< (hit `tag` detection v1 v2 (detectWall p1))
    
detection :: Velocity -> Velocity -> Bool -> Velocity
detection v1 _ True = (-2) *^ v1
detection v1 v2 False = 2 *^ afterColVel v1 v2

afterColVel :: Velocity -> Velocity -> Velocity
--afterColVel v1 v2 = ( (0.5 - elasS') +^(((-1) * elasS') *^ v1) ^+^ ( + (massA / massS) * 0.5) *^ v2)
afterColVel v1 v2 = (((-1) * elasS') *^ v1) ^+^ (((massA / massS) * 0.5) *^ v2)
        where 
         elasS' = elasS * (1 - 0) * 0.5
         elasS = 1
         massA = 1
         massS = 1
{-
VelS' = (ElasS' * VelS * (-1) + (0.5 - ElasS')) + (MassA `div` MassS) * VelA * 0.5
        where ElasS' = ElasS * (1 - IsProjectile(A)) * 0.5
        => Wie wir IsProjectile abfragen, müssen wir noch sehen. 
           Die höchste Elastizität, die das System unterstützt, ist damit 50% (1.0)
-}
    
    
wallCollisionSF :: SF (Position,Velocity) (Event Velocity)
wallCollisionSF = proc (p, v) -> do
    hit <- edge -< detectWall p
    returnA -< (hit `tag` ((-2) *^ v))

detectWall :: Position -> Bool
detectWall (Vector x y)
    | x < (-1) = True
    | x > 1 = True
    | y < (-1) = True
    | y > 1 = True
    | otherwise = False
    
accelerate :: Position -> Velocity -> SF Acceleration (Position,Velocity)
accelerate pos0 v0 = proc acc -> do
    v <- (v0^+^) ^<< integral -< acc
    pos <- (pos0^+^) ^<< integral -< v
    returnA -<(pos,v)
    
    
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
    actionRef <- newIORef AccNone
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
initGameState = [createPlayer, createEnemy (Vector 0.7 0.9)]
    

----------------- reactimate functions     ------------------
-- typed: ReactHandle Input Output -> ReactHandle () (Float,Float)

-- init react
initr :: IO Acceleration
initr = return zeroVector

--actuate react
actuate :: ReactHandle Acceleration GameState -> Bool -> GameState -> IO Bool
actuate _ _ gameState = do 
    renderScene gameState
    return False
    

----------------- openGL callbacks ---------------------------

display :: IO ()
display = do
    return ()
    
idle :: IORef UTCTime -> IORef Action -> ReactHandle Acceleration GameState -> IO ()
idle timeRef inputRef rh = do
    now <- getCurrentTime
    lastTime <- readIORef timeRef
    writeIORef timeRef now
    input <- readIORef inputRef
    let dt = now `diffUTCTime` lastTime
    react rh (realToFrac dt, Just (convAcc input))
    return ()
    
-- helper for converting input to acceleration
convAcc :: Action -> Acceleration
convAcc AccUp = Vector 0 1
convAcc AccDown = Vector 0 (-1)
convAcc AccLeft = Vector (-1) 0
convAcc AccRight = Vector 1 0
convAcc AccNone = Vector 0 0
    
resizeWindow :: Size -> IO ()
resizeWindow size = do
    -- TODO keep aspect ratio
    -- viewport $= (Graphics.UI.GLUT.Position 0 0, size)
    return ()
    
keyboardMouse :: IORef Action -> KeyboardMouseCallback
keyboardMouse inputRef key Down _ _ = case key of
    (SpecialKey KeyLeft) -> inputRef $~! \_ -> AccLeft
    (SpecialKey KeyRight) -> inputRef $~! \_ -> AccRight
    (SpecialKey KeyUp) -> inputRef $~! \_ -> AccUp
    (SpecialKey KeyDown) -> inputRef $~! \_ -> AccDown
    _ -> return ()
keyboardMouse inputRef key Up _ _ = inputRef $~! \_ -> AccNone
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

type Sprite = [Position] -- TODO rotation

renderScene :: GameState -> IO ()
renderScene [GameObject posS vs accS Player, GameObject posA _ _ Enemy] = do
    clear [ColorBuffer]
    loadIdentity
    --mapM_ renderGameObject gs
    renderPlayer posS
    renderEnemy posA
    print (posS ^-^ posA)
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
renderGameObject (GameObject pos _ _ Player) = renderPlayer pos
renderGameObject (GameObject pos _ _ Enemy) = renderEnemy pos

renderPlayer :: Position -> IO ()
renderPlayer pos = do
    renderPrimitive TriangleFan $ (color $ Color3 (1::GLfloat) (0::GLfloat) (0::GLfloat)) >> (mapM_ (\pos@(Vector x y) -> vertex $ Vertex2 x y) (translateTo playerPointSet pos))
    
renderEnemy :: Position -> IO ()
renderEnemy pos = do
    renderPrimitive TriangleFan $ (color $ Color3 (1::GLfloat) (1::GLfloat) (0::GLfloat)) >> (mapM_ (\pos@(Vector x y) -> vertex $ Vertex2 x y) (translateTo enemyPointSet pos))
    
playerPointSet :: Sprite
playerPointSet = let r=0.15 in map (\t -> Vector (r*cos(t)) (r*sin(t))) [0,0.2..(2*pi)]

enemyPointSet :: Sprite
enemyPointSet = let r=0.1 in map (\t -> Vector (r*cos(t)) (r*sin(t))) [0,0.2..(2*pi)]

translateTo :: Sprite -> Position -> Sprite
translateTo s pos = map (\pos' -> pos ^+^ pos') s

--convertPos :: Position -> GLPos
--convertPos (x,y) = (realToFrac x, realToFrac y)


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