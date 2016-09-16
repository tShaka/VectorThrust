{-# LANGUAGE Arrows #-}

module Game where

import FRP.Yampa
import Model
import Graphics.UI.GLUT hiding (position, Position, normalize)
import Data.List

-- creates initial GameState
initGameState :: GameState
initGameState = [createPlayer, createEnemy (Vector 0.2 0.2)]
--initGameState = [createPlayer, createEnemy (Vector 0.5 0.4), createEnemy (Vector 0.9 0.4)]

createPlayer :: GameObject
createPlayer = GameObject zeroVector zeroVector zeroVector 0 0 1 1 sizePlayer 10000 0 Player
    
createEnemy :: Position -> GameObject
createEnemy pos = GameObject pos zeroVector zeroVector 0 0 0.9 1 sizeEnemy 10000 0 Enemy


-- Hauptschleife für Bewegung aller GameObjects im GameState - berechnet Kollision und anschließend neue Position für alle GameObjects abhängig von Bewegung und Kollisionserkennung jedes GameObjects
mainGameSF :: GameState -> SF Action (GameState,[(Event Velocity, Event Position)])
mainGameSF gs = proc act -> do
    rec
        preGs <- iPre gs -< gs'
        let colEvents = collisionDetection preGs
        gs' <- gameSF' gs -< (act, colEvents)
    returnA -< (gs',colEvents)

{- 
mainGameSF :: GameState -> SF Action GameState
mainGameSF gs = proc act -> do
    rec
        preGs <- iPre gs -< gs'
        let colEvents = collisionDetection preGs
        gs' <- gameSF' gs -< (act, colEvents)
    returnA -< gs'-}
    
    
-- rekursive gameSF
gameSF' :: GameState -> SF (Action, [(Event Velocity, Event Position)]) GameState
gameSF' [] = proc (_,[]) -> returnA -< []
gameSF' (iObject : iObjects) = proc (actS, (deltaVel, deltaPos):events) -> do
    -- Player has acceleration depending on input - Enemies acceleration is constant
    let acc = if (objectType iObject) == Player then convAction actS else Vector (-0.1) (-0.1)    
    vS <- ((vel iObject) ^+^) ^<< impulseIntegral -< (acc, deltaVel)
    pS <- ((pos iObject) ^+^) ^<< impulseIntegral -< (vS, deltaPos)
    -- rekursiver aufruf
    gameStates <- gameSF' iObjects -< (actS, events) -- Rekursion!    
    returnA -< (GameObject pS vS acc (rot iObject) (spn iObject) (mas iObject) (ela iObject) (size iObject) (hp iObject) (dmg iObject) (objectType iObject)) : gameStates

-- helper for converting input to acceleration
convAction :: Action -> Acceleration
convAction action = convAcc (actionAcceleration action) ^+^ convTurn (actionTurn action)

convAcc :: ActionAcceleration -> Acceleration
convAcc AccUp = Vector 0 1
convAcc AccDown = Vector 0 (-1)
convAcc AccNone = Vector 0 0

convTurn :: ActionTurn -> Acceleration
convTurn TurnLeft = Vector (-1) 0
convTurn TurnRight = Vector 1 0
convTurn TurnNone = Vector 0 0    
    
-- Kollisionserkennung für alle Objekte im GameState - die Reihenfolge der Ausgabe entspricht der Eingabe
collisionDetection :: GameState -> [(Event Velocity, Event Position)]
collisionDetection gs = map (\o -> collision'' o gs) gs

-- Kollisionserkennung für ein GameObjekt - Kollision mit allen anderen GameObjects im GameState wird berechnet
collision' :: GameObject -> [GameObject] -> [(Event Velocity, Event Position)]
collision' o1 [] = []
collision' o1 (o2:os)
    | o1 == o2 = collision' o1 os
    | otherwise = collision o1 o2 : collision' o1 os
--TODO List Comprehensons/Filter
    
-- Kollisionserkennung für ein GameObjekt - Gibt das "größte" Kollisionsevent aus allen Kollisionen dieses GameObjects zurück
collision'' :: GameObject -> [GameObject] -> (Event Velocity, Event Position)
collision'' o1 os = biggestEvent $ filterNoEvents $ collision' o1 os

-- filtert aus gegebener Liste von Kollisionsevents alle NoEvents aus
filterNoEvents :: [(Event Velocity, Event Position)] -> [(Velocity, Position)]
filterNoEvents [] = []
filterNoEvents ((Event velocity, Event position):events) = (velocity, position): filterNoEvents events
filterNoEvents (_ : events) = filterNoEvents events

-- Wählt aus gegebener Liste von Kollisionsevents das "größte" aus
biggestEvent :: [(Velocity, Position)] -> (Event Velocity, Event Position)

biggestEvent [] = (NoEvent, NoEvent)
biggestEvent l = (Event vel, Event pos)
    where 
        (vel,pos) = maximumBy comparebyVelocity l
        comparebyVelocity (vel1, _) (vel2, _) = compare (norm vel1) (norm vel2)

-- neue Version testweise mit tupel output
collision :: GameObject -> GameObject -> (Event Velocity, Event Position)
collision o1 o2 = if isColliding (pos o1, pos o2) || detectWall (pos o1) || detectVertWall (pos o1) then detection o1 o2 (detectWall (pos o1)) (detectVertWall (pos o1)) else (NoEvent, NoEvent)    

--Überprüft die Kollision von zwei Objekten. Aktuell haben wir nur 2.
-- TODO: Andere Objekte als Kreise
-- TODO: distance durch norm ersetzen 
isColliding :: (Position,Position) -> Bool
isColliding (posS,posA) = ((distance dpos) - (rs + ra)) <= sigma
    where
        rs = sizePlayer
        ra = sizeEnemy
        dpos = posS ^-^ posA
        sigma = 0    
    
detectWall :: Position -> Bool
detectWall (Vector x y)
    | y < (-1.01) = True
    | y > 1.01 = True
    | otherwise = False

detectVertWall :: Position -> Bool
detectVertWall (Vector x y)
    | x < (-1.01) = True
    | x > 1.01 = True
    | otherwise = False    

distance :: Vector -> GLfloat
distance (Vector x y) = sqrt(x*x + y*y)
    

detection :: GameObject -> GameObject -> Bool -> Bool-> (Event Velocity, Event Position)
{-
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
-}
detection ob1 ob2 True True = (Event (Vector 0 0), Event ((-1.99) *^ (pos ob1)))
    where (Vector x y) = (vel ob1)
detection ob1 ob2 True False = (Event (Vector 0 0), Event (Vector 0 ((-1.99)*py)))
    where (Vector _ py) = (pos ob1)
detection ob1 ob2 False True = (Event (Vector 0 0), Event (Vector ((-1.99)*px) 0))
    where (Vector vx _) = (vel ob1)
          (Vector px _) = (pos ob1)
detection ob1 ob2 False False = afterColVel ob1 ob2

afterColVel :: GameObject -> GameObject -> (Event Velocity, Event Position)
afterColVel ob1 ob2 = 
        (Event ((
        --( ((mas ob2) / (mas ob1)) * (ela ob1) )
        1 *^ v2s) ^+^ v1p ^-^ (vel ob1)), Event ((1) *^ overlap))
        where 
         v1p = (vel ob1 `dot` normedDist) *^ normedDist
         v2p = (vel ob2 `dot` normedDist) *^ normedDist
         --v2p = (((vel ob2) `dot` dist) / (dist `dot` dist)) *^ dist
         v1s = (vel ob1) ^-^ v1p
         v2s = (vel ob2) ^-^ v2p
         overlap = dist ^-^  ((size ob1) + (size ob2)) *^ normedDist
         dist = (pos ob1) ^-^ (pos ob2) 
         normedDist = normalize dist
--TODO    Überall normalize, größe * 0.15     
