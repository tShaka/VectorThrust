{-# LANGUAGE Arrows #-}

module Game where

import FRP.Yampa
import Model
import Graphics.UI.GLUT hiding (position, Position)


-- creates initial GameState
initGameState :: GameState
initGameState = [createPlayer, createEnemy (Vector 0.5 0.4)]

createPlayer :: GameObject
createPlayer = GameObject zeroVector zeroVector zeroVector Player
    
createEnemy :: Position -> GameObject
createEnemy pos = GameObject pos zeroVector zeroVector Enemy


-- Hauptschleife für Bewegung aller GameObjects im GameState - berechnet Kollision und anschließend neue Position für alle GameObjects abhängig von Bewegung und Kollisionserkennung jedes GameObjects
mainGameSF :: GameState -> SF Acceleration GameState
mainGameSF gs = proc acc -> do
    rec
        preGs <- iPre gs -< gs'
        let colEvents = collisionDetection preGs
        gs' <- gameSF' gs -< (acc, colEvents)
    returnA -< gs'
    
-- rekursive gameSF
gameSF' :: GameState -> SF (Acceleration, [Event Velocity]) GameState
gameSF' [] = proc (_,[]) -> returnA -< []
gameSF' (iObject : iObjects) = proc (accS, event:events) -> do
    -- Player has acceleration depending on input - Enemies acceleration is constant
    let acc = if (objectType iObject) == Player then accS else Vector (-0.1) (-0.1) 
    vS <- ((vel iObject) ^+^) ^<< impulseIntegral -< (acc, event)
    pS <- ((pos iObject) ^+^) ^<< integral -< vS
    -- rekursiver aufruf
    gameStates <- gameSF' iObjects -< (acc, events) -- Rekursion!
    returnA -< (GameObject pS vS acc (objectType iObject)) : gameStates
    

-- Kollisionserkennung für alle Objekte im GameState - die Reihenfolge der Ausgabe entspricht der Eingabe
collisionDetection :: GameState -> [Event Velocity]
collisionDetection gs = map (\o -> collision'' o gs) gs

-- Kollisionserkennung für ein GameObjekt - Kollision mit allen anderen GameObjects im GameState wird berechnet
collision' :: GameObject -> [GameObject] -> [Event Velocity]
collision' o1 [] = []
collision' o1 (o2:os)
    | o1 == o2 = collision' o1 os
    | otherwise = collision o1 o2 : collision' o1 os

-- Kollisionserkennung für ein GameObjekt - Gibt das "größte" Kollisionsevent aus allen Kollisionen dieses GameObjects zurück
collision'' :: GameObject -> [GameObject] -> Event Velocity
collision'' o1 os = biggestEvent (collision' o1 os)

-- Wählt aus gegebener Liste von Kollisionsevents das "größte" aus
biggestEvent :: [Event Velocity] -> Event Velocity
biggestEvent [] = NoEvent
biggestEvent (e:[]) = e
biggestEvent (e1@(Event x) : e2@(Event y) : []) = if (x `dot` x) < (y `dot` y) then e2 else e1
biggestEvent (e1@(Event x) : e2@(Event y) : xs) = if (x `dot` x) < (y `dot` y) then biggestEvent (e2:xs) else biggestEvent (e1:xs)    
    
collision :: GameObject -> GameObject -> Event Velocity
collision o1 o2 = if isColliding (pos o1, pos o2) || detectWall (pos o1) || detectVertWall (pos o1) then Event (detection (vel o1) (vel o2) (pos o1) (pos o2) (detectWall (pos o1)) (detectVertWall (pos o1))) else NoEvent
    
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
    
detectWall :: Position -> Bool
detectWall (Vector x y)
    | y < (-1) = True
    | y > 1 = True
    | otherwise = False

detectVertWall :: Position -> Bool
detectVertWall (Vector x y)
    | x < (-1) = True
    | x > 1 = True
    | otherwise = False    
    
-- TODO: GameObject nutzen    
detection :: Velocity -> Velocity -> Position -> Position -> Bool -> Bool-> Velocity
detection v _ _ _ True True = (-2) *^ v
detection (Vector _ vy) _ _ _ True False = Vector 0 ((-2)*vy)
detection (Vector vx _) _ _ _ False True = Vector ((-2)*vx) 0
detection v1 v2 p1 p2 False False = afterColVel v1 v2 p1 p2

afterColVel :: Velocity -> Velocity -> Position -> Position -> Velocity
afterColVel v1 v2 p1 p2 = --(((-1) * elas1') *^ v1) ^+^ 
        (((mass2 / mass1) * (1)) *^ v2s) ^+^ v1p ^-^ v1
        where 
         --elas1' = elas1 * (1 - 0) * 0.1
         --elas1 = 1
         mass1 = 1
         mass2 = 1
         v1p = ((v1 `dot` dist) / (dist `dot` dist)) *^ dist
         v2p = ((v2 `dot` dist) / (dist `dot` dist)) *^ dist
         v1s = v1 ^-^ v1p
         v2s = v2 ^-^ v2p
         dist = p1 ^-^ p2
         --dist = distance dpos
         
distance :: Vector -> GLfloat
distance (Vector x y) = sqrt(x*x + y*y)