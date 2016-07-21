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
, GameObjectSpin(..)
, GameObjectMass(..)
, GameObjectElas(..)
, GameObjectSize(..)
, IsProjectile(..)
) where

import Graphics.UI.GLUT hiding (Position, Level)
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
}

data GameObjectType = Player | Enemy | Asteroid
-- alternatively instead of defining GameObjectType?
-- data GameObject = Player GameObjectMass | Enemy GameObjectMass

-- GameState == Level in this case
type GameState = [GameObject]

type Rotation = GLfloat
type GameObjectSpin = GLfloat
type GameObjectMass = GLfloat
type GameObjectSize = GLfloat
type GameObjectElas = GLfloat
--New input, couldn't test @univ:
type IsProjectile = Bool

data Action = AccLeft | AccRight | AccUp | AccDown | AccNone