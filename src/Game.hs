{-# LANGUAGE Arrows #-}

module Game where

import FRP.Yampa
import Model
import Graphics.UI.GLUT hiding (position, Position)


-- creates initial GameState
initGameState :: GameState
initGameState = [createPlayer, createEnemy (Vector 0.5 0.4), createEnemy (Vector 0.9 0.4)]

createPlayer :: GameObject
createPlayer = GameObject zeroVector zeroVector zeroVector 0 0 1 1 1 10000 0 Player
    
createEnemy :: Position -> GameObject
createEnemy pos = GameObject pos zeroVector zeroVector 0 0 0.9 1 0.8 10000 0 Enemy


-- Hauptschleife für Bewegung aller GameObjects im GameState - berechnet Kollision und anschließend neue Position für alle GameObjects abhängig von Bewegung und Kollisionserkennung jedes GameObjects
mainGameSF :: GameState -> SF Acceleration GameState
mainGameSF gs = proc acc -> do
    rec
        preGs <- iPre gs -< gs'
        let colEvents = collisionDetection preGs
        gs' <- gameSF' gs -< (acc, colEvents)
    returnA -< gs'
    
-- rekursive gameSF
gameSF' :: GameState -> SF (Acceleration, [(Event Velocity, Event Position)]) GameState
gameSF' [] = proc (_,[]) -> returnA -< []
gameSF' (iObject : iObjects) = proc (accS, (deltaVel, deltaPos):events) -> do
    -- Player has acceleration depending on input - Enemies acceleration is constant
    let acc = if (objectType iObject) == Player then accS else Vector (-0.1) (-0.1)    
    vS <- ((vel iObject) ^+^) ^<< impulseIntegral -< (acc, deltaVel)
    pS <- ((pos iObject) ^+^) ^<< impulseIntegral -< (vS, deltaPos)
    -- rekursiver aufruf
    gameStates <- gameSF' iObjects -< (acc, events) -- Rekursion!    
    returnA -< (GameObject pS vS acc (rot iObject) (spn iObject) (mas iObject) (ela iObject) (size iObject) (hp iObject) (dmg iObject) (objectType iObject)) : gameStates
    
    
-- Kollisionserkennung für alle Objekte im GameState - die Reihenfolge der Ausgabe entspricht der Eingabe
collisionDetection :: GameState -> [(Event Velocity, Event Position)]
collisionDetection gs = map (\o -> collision'' o gs) gs

-- Kollisionserkennung für ein GameObjekt - Kollision mit allen anderen GameObjects im GameState wird berechnet
collision' :: GameObject -> [GameObject] -> [(Event Velocity, Event Position)]
collision' o1 [] = []
collision' o1 (o2:os)
    | o1 == o2 = collision' o1 os
    | otherwise = collision o1 o2 : collision' o1 os

-- Kollisionserkennung für ein GameObjekt - Gibt das "größte" Kollisionsevent aus allen Kollisionen dieses GameObjects zurück
collision'' :: GameObject -> [GameObject] -> (Event Velocity, Event Position)
collision'' o1 os = biggestEvent $ filterNoEvents $ collision' o1 os

-- filtert aus gegebener Liste von Kollisionsevents alle NoEvents aus
filterNoEvents :: [(Event Velocity, Event Position)] -> [(Event Velocity, Event Position)]
filterNoEvents events = filter (\(e,_) -> not (e == NoEvent)) events

-- Wählt aus gegebener Liste von Kollisionsevents das "größte" aus
biggestEvent :: [(Event Velocity, Event Position)] -> (Event Velocity, Event Position)
biggestEvent [] = (NoEvent, NoEvent)
biggestEvent (e:[]) = e
biggestEvent (e1@(Event x, _) : e2@(Event y, _) : []) = if (x `dot` x) < (y `dot` y) then e2 else e1
biggestEvent (e1@(Event x, _) : e2@(Event y, _) : xs) = if (x `dot` x) < (y `dot` y) then biggestEvent (e2:xs) else biggestEvent (e1:xs)    
    
-- neue Version testweise mit tupel output
collision :: GameObject -> GameObject -> (Event Velocity, Event Position)
collision o1 o2 = if isColliding (pos o1, pos o2) || detectWall (pos o1) || detectVertWall (pos o1) then detection o1 o2 (detectWall (pos o1)) (detectVertWall (pos o1)) else (NoEvent, NoEvent)    

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

distance :: Vector -> GLfloat
distance (Vector x y) = sqrt(x*x + y*y)
    
{- 
detection :: Velocity -> Velocity -> Position -> Position -> Bool -> Bool-> (Event Velocity, Event Position)
detection v _ (Vector x y) _ True True
    | x < (-1) && y < (-1) = (Event ((-2) *^ v), Event (Vector (0.001) (0.001)))
    | x > 1 && y < (-1) = (Event ((-2) *^ v), Event (Vector (-0.001) (0.01)))
    | x < (-1) && y > 1 = (Event ((-2) *^ v), Event (Vector (0.001) (-0.001)))
    | x > 1 && y > 1 = (Event ((-2) *^ v), Event (Vector (-0.001) (-0.001)))
detection (Vector _ vy) _ (Vector _ y) _ True False = if y < (-1) then (Event (Vector 0 ((-2)*vy)), Event (Vector 0 0.001)) else (Event (Vector 0 ((-2)*vy)), Event (Vector 0 (-0.001)))
detection (Vector vx _) _ (Vector x _) _ False True = if x < (-1) then (Event (Vector ((-2)*vx) 0), Event (Vector 0.001 0)) else (Event (Vector ((-2)*vx) 0), Event (Vector (-0.001) 0))
detection v1 v2 p1 p2 False False = afterColVel' v1 v2 p1 p2
-}

detection :: GameObject -> GameObject -> Bool -> Bool-> (Event Velocity, Event Position)
detection ob1 ob2 True True
    | x < (-1) && y < (-1) = (Event ((-2) *^ (vel ob1)), Event (Vector (0.001) (0.001)))
    | x > 1 && y < (-1) = (Event ((-2) *^ (vel ob1)), Event (Vector (-0.001) (0.01)))
    | x < (-1) && y > 1 = (Event ((-2) *^ (vel ob1)), Event (Vector (0.001) (-0.001)))
    | x > 1 && y > 1 = (Event ((-2) *^ (vel ob1)), Event (Vector (-0.001) (-0.001)))
    where (Vector x y) = (pos ob1)
detection ob1 ob2 True False = if py < (-1) then (Event (Vector 0 ((-2)*vy)), Event (Vector 0 0.001)) else (Event (Vector 0 ((-2)*vy)), Event (Vector 0 (-0.001)))
    where (Vector _ vy) = (vel ob1)
          (Vector _ py) = (pos ob1)
detection ob1 ob2 False True = if px < (-1) then (Event (Vector ((-2)*vx) 0), Event (Vector 0.001 0)) else (Event (Vector ((-2)*vx) 0), Event (Vector (-0.001) 0))
    where (Vector vx _) = (vel ob1)
          (Vector px _) = (pos ob1)
detection ob1 ob2 False False = afterColVel ob1 ob2

-- nur zum Testen der gameSF -> nicht die richtige Kollisionsformel -> wieder löschen !!!
afterColVel' :: Velocity -> Velocity -> Position -> Position -> (Event Velocity, Event Position)
afterColVel' v1 v2 p1 p2 = (Event ((-2)*^v1), Event (p1 ^-^ (Vector 0.001 0.001)))

afterColVel :: GameObject -> GameObject -> (Event Velocity, Event Position)
afterColVel ob1 ob2 = 
        (Event (((((mas ob2) / (mas ob1)) * (1)) *^ v2s) ^+^ v1p ^-^ (vel ob1)), Event ((-0.1) *^ dist))
        where 
         --elas1' = elas1 * (1 - 0) * 0.1
         --elas1 = 1
         --mass1 = 1
         --mass2 = 1
         v1p = (((vel ob1) `dot` dist) / (dist `dot` dist)) *^ dist
         v2p = (((vel ob2) `dot` dist) / (dist `dot` dist)) *^ dist
         v1s = (vel ob1) ^-^ v1p
         v2s = (vel ob2) ^-^ v2p
         dist = (pos ob1) ^-^ (pos ob2)
         
