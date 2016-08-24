{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model
( Velocity(..)
, Vector(..)
, Acceleration(..)
, Position(..)
, Action(..)
, ActionAcceleration(..)
, ActionTurn(..)
, actionNone
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
} deriving (Show, Eq)
        

data GameObjectType = Player | Enemy | Asteroid | Projectile
    deriving (Eq, Show)
-- alternatively instead of defining GameObjectType?
-- data GameObject = Player GameObjectMass | Enemy GameObjectMass

-- Radius der Spielergrafik
sizePlayer :: GLfloat
sizePlayer = 0.15

-- Radius der Gegnergrafik
sizeEnemy :: GLfloat
sizeEnemy = 0.10

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

data Action = Action {
    actionAcceleration :: ActionAcceleration, 
    actionTurn :: ActionTurn
    }
    deriving (Eq, Show)
    
actionNone :: Action
actionNone = Action AccNone TurnNone

data ActionAcceleration = AccUp | AccDown | AccNone
    deriving (Eq, Show)

data ActionTurn = TurnLeft | TurnRight | TurnNone
    deriving (Eq, Show)