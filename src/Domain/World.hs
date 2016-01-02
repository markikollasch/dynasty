module Domain.World
( Coord
, Side
, Tile
, Unit
) where

type Coord = (Int, Int)

data Side = Player | Enemy deriving (Eq, Show)

data Tile = Floor | Obstacle | Forbidden deriving (Show)

data Unit = Unit { name :: String
                 , side :: Side
                 } deriving (Eq, Show)
