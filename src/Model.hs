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
, IsProjectile(..)
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
    objectType :: GameObjectType
} deriving Show
instance Eq GameObject where
    GameObject pos1 vel1 acc1 type1 == GameObject pos2 vel2 acc2 type2
        | pos1 == pos2 && vel1 == vel2 && acc1 == acc2 && type1 == type2 = True
        | otherwise = False

data GameObjectType = Player | Enemy | Asteroid
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
type IsProjectile = Bool

data Action = AccLeft | AccRight | AccUp | AccDown | AccNone