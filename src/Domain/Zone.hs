module Domain.Zone
( Zone(..)
, ZoneTile(..)
, blank
, allUnits
, allTiles
, zoneTiles

, inBounds
, outOfBounds
, unitAt
, tileAt
, setUnit
, setTile
, containsUnit
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Domain.World

data Zone = Zone { size :: Coord
                 , units :: Map.Map Coord Unit
                 , tiles :: Map.Map Coord Tile
                 } deriving (Eq)

data ZoneTile a = ZoneTile { location :: Coord
                           , terrain :: Maybe Tile
                           , occupant :: Maybe Unit
                           , datum :: a
                           } deriving (Eq, Show)

zoneTiles :: Zone -> (Zone -> Coord -> a) -> [ZoneTile a]
zoneTiles zone func =
    let bounds = size zone
        allCoords = [ (x, y) | x <- [0..fst bounds - 1], y <- [0..snd bounds - 1] ]
        makeZoneTile :: (Zone -> Coord -> a) -> Coord -> ZoneTile a
        makeZoneTile f c = ZoneTile { location = c
                                    , terrain = tileAt zone c
                                    , occupant = unitAt zone c
                                    , datum = f zone c
                                    }
    in  map (makeZoneTile func) allCoords

blank :: Int -> Int -> Zone
blank w h =
    let coord = (max 1 w, max 1 h)
        allCoords = Set.fromList [ (x, y) | x <- [0..fst coord - 1], y <- [0..snd coord - 1] ]
    in Zone { size = coord
            , units = Map.empty
            , tiles = Map.fromSet (\_ -> Floor) allCoords
            }

allUnits :: Zone -> [(Coord, Unit)]
allUnits zone = Map.assocs $ units zone

allTiles :: Zone -> [(Coord, Tile)]
allTiles zone = Map.assocs $ tiles zone

inBounds :: Zone -> Coord -> Bool
inBounds zone coord
    | x < 0 || y < 0 = False
    | x < xMax && y < yMax = True
    | otherwise = False
    where x = fst coord
          y = snd coord
          xMax = fst $ size zone
          yMax = snd $ size zone

outOfBounds :: Zone -> Coord -> Bool
outOfBounds zone coord = not $ inBounds zone coord

unitAt :: Zone -> Coord -> Maybe Unit
unitAt zone coord
    | outOfBounds zone coord = Nothing
    | otherwise = Map.lookup coord $ units zone

tileAt :: Zone -> Coord -> Maybe Tile
tileAt zone coord
    | outOfBounds zone coord = Nothing
    | otherwise = Map.lookup coord $ tiles zone

setUnit :: Zone -> Coord -> Unit -> Zone
setUnit zone coord unit
    | outOfBounds zone coord = zone
    | otherwise = Zone { size = size zone
                       , units = Map.insert coord unit $ units zone
                       , tiles = tiles zone
                       }

setTile :: Zone -> Coord -> Tile -> Zone
setTile zone coord tile
    | outOfBounds zone coord = zone
    | otherwise = Zone { size = size zone
                       , units = units zone
                       , tiles = Map.insert coord tile $ tiles zone
                       }

containsUnit :: Zone -> Unit -> Bool
containsUnit zone unit = elem unit $ Map.elems $ units zone
