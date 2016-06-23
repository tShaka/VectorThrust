module Model
( Velocity(..)
, Position(..)
, GameObjectMass(..)
, GameObject(..)
, GameObjectType(..)
, Level(..)
, Rotation(..)
) where

import Graphics.UI.GLUT
import Yampa.Vec


data Vector = Vector GLfloat GLfloat
instance VectorSpacepace 
type Velocity = Vector
type Position = Vector

-- maybe incorporate radius
data GameObjectMass = GameObjectMass Position Velocity

data GameObject = GameObject GameObjectMass GameObjectType
data GameObjectType = Player | Enemy
-- alternatively instead of defining GameObjectType?
-- data GameObject = Player GameObjectMass | Enemy GameObjectMass

data Level = Level [GameObject]

data Rotation = Rotation GLfloat

