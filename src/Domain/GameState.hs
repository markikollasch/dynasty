module Domain.GameState
( GameState
, Instruction(..)
, updateGame
) where

import Control.Monad.State
import Domain.World
import Domain.Zone

data GameState = GameState { zone :: Maybe Zone
                           } -- TODO

data Instruction = DoNothing -- TODO

updateGame :: Instruction -> GameState -> GameState
updateGame DoNothing state = state
