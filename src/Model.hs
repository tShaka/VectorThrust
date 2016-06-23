module Model
( Velocity(..)
, Position(..)
, GObjectMass(..)
, GObject(..)
, GObjectType(..)
, Level(..)
, Rotation(..)
) where

import Graphics.UI.GLUT
-- TODO:
-- make Velocity and Position instances of Vectorspace

data Velocity = Velocity GLfloat GLfloat

data Position = Position GLfloat GLfloat

-- maybe incorporate radius
data GObjectMass = GObjectMass Position Velocity

data GObject = GObject GObjectMass GObjectType
data GObjectType = Player | Enemy
-- alternatively instead of defining GObjectType?
-- data GObject = Player GObjectMass | Enemy GObjectMass

data Level = Level [GObject]

data Rotation = Rotation GLfloat

