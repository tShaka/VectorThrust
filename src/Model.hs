module Model
( Velocity(..)
, Position(..)
, MassPoint(..)
, Object(..)
, ObjectType(..)
, Level(..)
, Rotation(..)
) where

-- TODO:
-- make Velocity and Position instances of Vectorspace

data Velocity = Velocity Double Double

data Position = Position Float Float

-- maybe incorporate radius
data MassPoint = MassPoint Position Velocity

data Object = Object MassPoint ObjectType
data ObjectType = Player | Enemy
-- alternatively instead of defining ObjectType?
-- data Object = Player MassPoint | Enemy MassPoint

data Level = Level [Object]

data Rotation = Rotation Int