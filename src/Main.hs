--
-- Copyright (C) 2016 Mark Kollasch
--
module Main
( main
) where

import System.Environment
import System.Clock
import Control.Concurrent
import Interface.InterfaceState
import Domain.GameState

main :: IO ()
main = do
    startTime <- getTime Monotonic
    mainLoop (initializeEnvironment startTime) initializeState


elapsed :: Environment -> TimeSpec
elapsed env = diffTimeSpec (current env) (start env)

delta :: Environment -> TimeSpec
delta env = diffTimeSpec (current env) (previous env)

updateEnvironment :: Environment -> IO Environment
updateEnvironment env = do
    newFrame <- getTime (clock env)
    return Environment { clock = clock env
                       , start = start env
                       , current = newFrame
                       , previous = current env }

mainLoop :: Environment -> InterfaceState -> IO ()
mainLoop oldEnvironment state = do
    env <- updateEnvironment oldEnvironment
    updatedState <- update env state
    render env updatedState
    if shouldExit env updatedState
        then return ()
        else mainLoop env updatedState

update :: Environment -> InterfaceState -> IO InterfaceState
update env state = do
    putStrLn "Updating...."
    threadDelay 100000
    return $ updateInterface NoInput nanosecs state -- TODO
    where nanosecs = timeSpecAsNanoSecs $ delta env

render :: Environment -> InterfaceState -> IO ()
render env state = do
    putStrLn "Rendering...."
    threadDelay 200000
    putStrLn $ ((dummyState . gameState) state) ++ " " ++ show (frameLength state)

shouldExit :: Environment -> InterfaceState -> Bool
--shouldExit _ _ = True
shouldExit env _ = (elapsed env) > (TimeSpec { sec = 3, nsec = 0 })

data Environment = Environment { clock :: Clock
                               , previous :: TimeSpec
                               , current :: TimeSpec
                               , start :: TimeSpec
                               } deriving (Show)

initializeState :: InterfaceState
initializeState = InterfaceState { gameState = GameState { zone = Nothing
                                                         , dummyState = "This frame lasted"
                                                         }
                                 , frameLength = 0
                                 }

initializeEnvironment :: TimeSpec -> Environment
initializeEnvironment start = Environment { clock = Monotonic
                                          , current = start
                                          , previous = start
                                          , start = start
                                          }
