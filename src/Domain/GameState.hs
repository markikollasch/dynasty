module Domain.GameState
( GameState(..)
, Instruction(..)
, updateGame
) where

import Control.Monad.State
import Domain.World
import Domain.Zone

data GameState = GameState { zone :: Maybe Zone
                           , dummyState :: String
                           , framesElapsed :: Integer
                           } -- TODO

data Instruction = DoNothing | DoSomething -- TODO

updateGame :: Instruction -> GameState -> GameState
updateGame DoNothing state = state
updateGame action state = let frameCount = succ $ framesElapsed state
                          in  GameState { zone = zone state
                                        , dummyState = "I've been updated " ++ show frameCount ++ " times!"
                                        , framesElapsed = frameCount
                                        }
