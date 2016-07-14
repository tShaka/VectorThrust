-- Ab jetzt (13.07.16) ist die GameWIp zum auslagern von noch nicht funktionalem Code gedacht.

{- TODO: Hier müssen wir die korrekte neue Geschwindigkeit berechnen. 
Dafür müssen wir Rotation, GameObjectMass, und GameObjectElas übergenen; 
Die Größe wird bereits für die Kollisionserkennung verwendet
-}
collisionSF :: SF (Position,Velocity,Position,Velocity) (Event Velocity)
collisionSF = proc (p1,v1,p2,v2) -> do
    hit <- edge -< isColliding (p1,p2) || detectWall p1 || detectVertWall p1
    returnA -< (hit `tag` detection v1 v2 (detectWall p1) (detectVertWall p1))
    
detection :: Velocity -> Velocity -> Bool -> Bool-> Velocity
detection v1 _ True False = (Vector (1 (-2))) `dot` v1
detection v1 _ False True = (Vector ((-2) 1)) `dot` v1
detection v1 v2 False False = 2 *^ afterColVel v1 v2

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
    | y < (-1) = True
    | y > 1 = True
    | otherwise = False

detectVertWall :: Position -> Bool
detectVertWall (Vector x y)
    | x < (-1) = True
    | x > 1 = True
    | otherwise = False