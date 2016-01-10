--
-- Copyright (C) 2016 Mark Kollasch
--
module Main
( main
) where

import System.Environment
import System.Clock
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
mainLoop oldEnvironment oldState = do
    newEnvironment <- updateEnvironment oldEnvironment
    newState <- return $ update newEnvironment oldState
    if shouldRender newEnvironment newState
        then putStrLn $ render newState
        else return ()
    if shouldExit oldEnvironment newState
        then return ()
        else mainLoop newEnvironment newState

update :: Environment -> InterfaceState -> InterfaceState
update env state = updateInterface NoInput nanosecs state -- TODO
    where nanosecs = timeSpecAsNanoSecs $ delta env

render :: InterfaceState -> String
render state =  text ++ " " ++ duration -- TODO: show something more meaningful than the number of nanoseconds since the start of the frame
    where duration = show $ frameLength state
          game = gameState state
          text = dummyState game

shouldExit :: Environment -> InterfaceState -> Bool
shouldExit env _ = (elapsed env) > (TimeSpec { sec = 1, nsec = 0 })

shouldRender :: Environment -> InterfaceState -> Bool
shouldRender env state = (frameLength state) <= (timeSpecAsNanoSecs $ delta env)

data Environment = Environment { clock :: Clock
                               , previous :: TimeSpec
                               , current :: TimeSpec
                               , start :: TimeSpec
                               } deriving (Show)

initializeState :: InterfaceState
initializeState = InterfaceState { gameState = GameState { zone = Nothing
                                                         , dummyState = "This frame lasted"
                                                         }
                                 , tickLength = 0
                                 , frameLength = 0
                                 }

initializeEnvironment :: TimeSpec -> Environment
initializeEnvironment start = Environment { clock = Monotonic
                                          , current = start
                                          , previous = start
                                          , start = start
                                          }
