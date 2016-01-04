module Domain.Zone.Tests
( tests
) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Domain.Zone

tests = testGroup "Zone tests"
    [ testCase "Demonstration test" testDemo
    ]

testDemo :: Assertion
testDemo =
    let zone = blank 1 1
    in assertBool "Expected (1,1) to be out of bounds in a 1x1 zone." (outOfBounds zone (1, 1))
