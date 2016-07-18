-- Ab jetzt (13.07.16) ist die GameWIp zum auslagern von noch nicht funktionalem Code gedacht.

{- TODO: Hier müssen wir die korrekte neue Geschwindigkeit berechnen. 
Dafür müssen wir Rotation, GameObjectMass, und GameObjectElas übergenen; 
Die Größe wird bereits für die Kollisionserkennung verwendet
-}
--collisionSF :: SF (Position,Velocity,Position,Velocity) (Event Velocity)
--collisionSF = proc (p1,v1,p2,v2) -> do
--    hit <- edge -< isColliding (p1,p2) || detectWall p1 || detectVertWall p1
-    returnA -< (hit `tag` detection v1 v2 (detectWall p1) (detectVertWall p1))
collisionSF :: SF (Position,Velocity,Position,Velocity) (Event Velocity)
collisionSF = proc (p1,v1,p2,v2) -> do
    returnA -< if isColliding (p1,p2) || detectWall p1 || detectVertWall p1 then Event (detection v1 v2 p1 p2 (detectWall p1) (detectVertWall p1)) else NoEvent

-- TODO: GameObject nutzen    
detection :: Velocity -> Velocity -> Position -> Position -> Bool -> Bool-> Velocity
detection (Vector _ vy) _ _ _ True False = Vector 0 ((-2)*vy)
detection (Vector vx _) _ _ _ False True = Vector ((-2)*vx) 0
detection v1 v2 p1 p2 False False = 2 *^ afterColVel v1 v2 p1 p2

collision :: GameObject -> GameObject -> Event Velocity
collision o1 o2 = if isColliding (pos o1, pos o2) || detectWall (pos o1) || detectVertWall (pos o1) then Event (detection (vel o1) (vel o2) (detectWall (pos o1)) (detectVertWall (pos o1))) else NoEvent

afterColVel :: Velocity -> Velocity -> Position -> Position -> Velocity
afterColVel v1 v2 p1 p2 = (((-1) * elasS') *^ v1) ^+^ (((massA / massS) * (-0.9)) *^ v1p)
        where 
         elasS' = elasS * (1 - 0) * 0.1
         elasS = 1
         massA = 1
         massS = 1
         v1p = ((v1 `dot` dist) / (dist `dot` dist)) *^ dist
         v1s = v1 ^-^ v1p
         dist = p1 ^-^ p2
         --dist = distance dpos
{-
afterColVel :: Velocity -> Velocity -> Velocity
afterColVel v1 v2 = (((-1) * elasS') *^ v1) ^+^ (((massA / massS) * 0.5) *^ v2)
        where 
         elasS' = elasS * (1 - 0) * 0.5
         elasS = 1
         massA = 1
         massS = 1
         -}
{-
VelS' = (ElasS' * VelS * (-1) + (0.5 - ElasS')) + (MassA `div` MassS) * VelA * 0.5
        where ElasS' = ElasS * (1 - IsProjectile(A)) * 0.5
        => Wie wir IsProjectile abfragen, müssen wir noch sehen. 
           Die höchste Elastizität, die das System unterstützt, ist damit 50% (1.0)
-}

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