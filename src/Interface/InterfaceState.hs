module Interface.InterfaceState
( InterfaceState
, Input
, updateInterface
) where

import Domain.GameState

data InterfaceState = InterfaceState { gameState :: GameState
                                     } -- TODO

data Input = NoInput | SomeInput -- TODO

updateInterface :: Input -> InterfaceState -> InterfaceState
updateInterface NoInput state = state
updateInterface input state = let newGameState = updateGame DoNothing (gameState state)
                               in InterfaceState { gameState = newGameState }
