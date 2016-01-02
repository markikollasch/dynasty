module Domain.Zone
( Zone
, inBounds
, unitAt
, tileAt
, addUnit
, addTile
, containsUnit
) where

import qualified Data.Map.Strict as Map
import Domain.World

data Zone = Zone { size :: Coord
                 , units :: Map.Map Coord Unit
                 , tiles :: Map.Map Coord Tile
                 }

inBounds :: Zone -> Coord -> Bool
inBounds zone coord
    | x < 0 || y < 0 = False
    | x < xMax && y < yMax = True
    | otherwise = False
    where x = fst coord
          y = snd coord
          xMax = fst (size zone)
          yMax = snd (size zone)

outOfBounds :: Zone -> Coord -> Bool
outOfBounds zone coord = not (inBounds zone coord)

unitAt :: Zone -> Coord -> Maybe Unit
unitAt zone coord
    | outOfBounds zone coord = Nothing
    | otherwise = Map.lookup coord (units zone)

tileAt :: Zone -> Coord -> Maybe Tile
tileAt zone coord
    | outOfBounds zone coord = Nothing
    | otherwise = Map.lookup coord (tiles zone)

addUnit :: Zone -> Coord -> Unit -> Zone
addUnit zone coord unit
    | outOfBounds zone coord = zone
    | otherwise = Zone { size = size zone
                       , units = Map.insert coord unit (units zone)
                       , tiles = tiles zone
                       }

addTile :: Zone -> Coord -> Tile -> Zone
addTile zone coord tile
    | outOfBounds zone coord = zone
    | otherwise = Zone { size = size zone
                       , units = units zone
                       , tiles = Map.insert coord tile (tiles zone)
                       }

containsUnit :: Zone -> Unit -> Bool
containsUnit zone unit = elem unit (Map.elems (units zone))
