module Domain.Zone.Tests
( tests
) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Domain.Zone
import Domain.World

tests = testGroup "Zone tests"
    [ testCase "Demonstration test" testDemo
    , testCase "Zone Tiles test" testTransform
    ]

testDemo :: Assertion
testDemo =
    let zone = blank 1 1
    in assertBool "Expected (1,1) to be out of bounds in a 1x1 zone." (outOfBounds zone (1, 1))

testTransform :: Assertion
testTransform =
    let myGuy = Unit { name = "Blast Hardcheese", side = Player }
        initial = blank 2 2
        spec = [ ((0, 0), Floor)
               , ((0, 1), Floor)
               , ((1, 0), Obstacle)
               , ((1, 1), Obstacle)
               ]
        tiles = foldl (\z (c, t) -> setTile z c t) initial spec
        zone = setUnit tiles (0, 1) myGuy
        result = zoneTiles zone (\z c -> ())
        expected = [ ZoneTile { location = (0, 0)
                              , terrain = Just Floor
                              , occupant = Nothing
                              , datum = ()}
                   , ZoneTile { location = (0, 1)
                              , terrain = Just Floor
                              , occupant = Just myGuy
                              , datum = ()}
                   , ZoneTile { location = (1, 0)
                              , terrain = Just Obstacle
                              , occupant = Nothing
                              , datum = ()}
                   , ZoneTile { location = (1, 1)
                              , terrain = Just Obstacle
                              , occupant = Nothing
                              , datum = ()}
                   ]
        in  assertEqual "Expected zone tiles to be assembled correctly" expected result
