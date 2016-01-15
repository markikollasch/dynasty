module Domain.Character
( Character(..)
, Stats(..)
) where

import Domain.World

data Character = Character { name :: String
                           , experience :: Integer
                           , side :: Side
                           , stats :: PlaceholderStats
                           } deriving (Show, Eq)

data Stats = PlaceholderStats {}
