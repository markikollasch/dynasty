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
import Hate

load :: LoadFn InterfaceState
load = return $ InterfaceState { gameState = GameState { zone = Nothing
                                                       , dummyState = "I've just started!"
                                                       , framesElapsed = 0
                                                       }
                               , tickLength = 0
                               , frameLength = 0
                               }

config :: Config
config = Config
    { windowTitle = "Hello world"
    , windowSize = (640, 480)
    }

draw :: DrawFn InterfaceState
draw state = []

update :: UpdateFn InterfaceState
update events = do
    return ()

main :: IO ()
main = runApp config load update draw
