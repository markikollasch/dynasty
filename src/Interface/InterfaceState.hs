module Interface.InterfaceState
( InterfaceState(..)
, Input(..)
, updateInterface
) where

import Domain.GameState

data InterfaceState = InterfaceState { gameState :: GameState
                                     , tickLength :: Integer
                                     , frameLength :: Integer
                                     } -- TODO

data Input = NoInput | SomeInput -- TODO

updateInterface :: Input -> Integer -> InterfaceState -> InterfaceState
updateInterface _ delta state = let newGameState = updateGame DoNothing (gameState state)
                                    before = frameLength state
                                    after = before + delta
                                    frame = after `mod` 33333333 -- TODO: get rid of this magic number
                                    in InterfaceState { gameState = newGameState
                                                      , tickLength = delta
                                                      , frameLength = frame
                                                      }
