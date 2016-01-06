--
-- Copyright (C) 2016 Mark Kollasch
--
module Main
( main
) where

import System.Environment
import Interface.InterfaceState
import Domain.GameState

main :: IO ()
main = mainLoop initializeState

mainLoop :: InterfaceState -> IO ()
mainLoop state = do
    updatedState <- update state
    render updatedState
    if shouldExit updatedState
        then return ()
        else mainLoop updatedState

update :: InterfaceState -> IO InterfaceState
update state = return $ updateInterface NoInput state -- TODO

render :: InterfaceState -> IO ()
render state = putStrLn $ (dummyState . gameState) state

shouldExit :: InterfaceState -> Bool
shouldExit _ = True -- TODO

initializeState :: InterfaceState
initializeState = InterfaceState { gameState = GameState { zone = Nothing
                                                         , dummyState = "Nothing to see here yet."
                                                         }
                                 }
