module Main
  ( main
  ) where

import Brick
import qualified Graphics.Vty as V

main :: IO ()
main = do
  chan <- newBChan 20
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 150000 -- game move frequency
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initVty <- builder
  void $ customMain initVty builder (Just chan) app g

-- Events Handler
handleEvent :: BrickEvent Name Tick -> EventM Name Game ()
handleEvent (AppEvent Tick)                       = do
                                                        g <- get
                                                        put $ updateState g

-- Game control exit/restart
-- TODO: handle in game moudle

handleEvent (VtyEvent (V.EvKey V.KEsc []))        = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = do
                                                      g <- get 
                                                      put $ restart g

handleEvent (VtyEvent (V.EvKey (V.KChar 'y') [])) = do
                                                      g <- get 
                                                      put $ continue g

-- TODO: Handle control keys: w a s d 

-- Hanlde undifined keys
handleEvent _                                     = do
                                                      g <- get
                                                      put g

-- TODO: Update game state