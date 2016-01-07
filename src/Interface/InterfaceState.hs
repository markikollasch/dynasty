module Interface.InterfaceState
( InterfaceState(..)
, Input(..)
, updateInterface
) where

import Domain.GameState

data InterfaceState = InterfaceState { gameState :: GameState
                                     , frameLength :: Integer
                                     } -- TODO

data Input = NoInput | SomeInput -- TODO

updateInterface :: Input -> Integer -> InterfaceState -> InterfaceState
updateInterface _ delta state = let newGameState = updateGame DoNothing (gameState state)
                                    in InterfaceState { gameState = newGameState
                                                      , frameLength = delta }
