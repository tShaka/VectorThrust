{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model
( Velocity(..)
, Vector(..)
, Acceleration(..)
, Position(..)
, Action(..)
, GameObject(..)
, GameObjectType(..)
, GameState(..)
, Rotation(..)
, Spin(..)
, Mass(..)
, Elasticity(..)
, ObjectSize(..)
, Health(..)
, Damage(..)
) where

import Graphics.UI.GLUT hiding (Position, Level, Size)
import FRP.Yampa.VectorSpace


data Vector = Vector GLfloat GLfloat
    deriving Show
instance VectorSpace Vector GLfloat where
    zeroVector = Vector 0 0
    a *^ Vector x y = Vector (x*a) (y*a)
    Vector x1 y1 ^+^ Vector x2 y2 = Vector (x1+x2) (y1+y2)
    Vector x1 y1 ^-^ Vector x2 y2 = Vector (x1-x2) (y1-y2)
    Vector x1 y1 `dot` Vector x2 y2 = x1*x2 + y1*y2
    --Vector x1 y1 `cross` Vector x2 y2 = Vector (x1*x2) (y1*y2)
instance Eq Vector where
    Vector x1 y1 == Vector x2 y2
        | x1 == x2 && y1 == y2 = True
        | otherwise = False

type Velocity = Vector
type Position = Vector
type Acceleration = Vector

-- maybe incorporate radius
-- data GameObjectMass = GameObjectMass

-- old version: data GameObject = GameObject Position Velocity Acceleration GameObjectType -- TODO + mass !
data GameObject = GameObject {
    pos :: Position,
    vel :: Velocity,
    acc :: Acceleration,
    rot :: Rotation,
    spn :: Spin,
    mas :: Mass, 
    ela :: Elasticity,
    size :: ObjectSize,
    hp :: Health,
    dmg :: Damage,
    objectType :: GameObjectType
} deriving Show
instance Eq GameObject where
    GameObject pos1 vel1 acc1 rot1 spn1 mas1 ela1 size1 hp1 dmg1 type1 == GameObject pos2 vel2 acc2 rot2 spn2 mas2 ela2 size2 hp2 dmg2 type2
        | pos1 == pos2 && vel1 == vel2 && acc1 == acc2 && rot1 == rot2 && spn1 == spn2 && mas1 == mas2 &&  ela1 == ela2 && size1 == size2 && hp1 == hp2 && dmg1 == dmg2 && type1 == type2 = True
        | otherwise = False

data GameObjectType = Player | Enemy | Asteroid | Projectile
    deriving (Eq, Show)
-- alternatively instead of defining GameObjectType?
-- data GameObject = Player GameObjectMass | Enemy GameObjectMass

-- GameState == Level in this case
type GameState = [GameObject]

type Rotation = GLfloat
type Spin = GLfloat
type Mass = GLfloat
type ObjectSize = GLfloat
type Elasticity = GLfloat
type Health = GLfloat
type Damage = GLfloat
--New input, couldn't test @univ:
--type IsProjectile = Bool

data Action = AccLeft | AccRight | AccUp | AccDown | AccNone