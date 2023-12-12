module Main
  ( main
  ) where

import Model
import View
import Control
import Brick
import qualified Graphics.Vty as V
import Control.Monad (forever, void)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (threadDelay, forkIO)

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

-- Application initialization
app :: App Game Tick Name
app = App { appDraw = drawApp
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = do
                              g <- get
                              put g
          , appAttrMap = const attributeMap
          }

-- Events Handler

handleEvent :: BrickEvent Name Tick -> EventM Name Game ()
handleEvent (AppEvent Tick)                       = do
                                                        g <- get
                                                        put $ updateState g

-- Game control exit/restart

handleEvent (VtyEvent (V.EvKey V.KEsc []))        = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = do
                                                      g <- get 
                                                      put $ restart g

handleEvent (VtyEvent (V.EvKey (V.KChar 'y') [])) = do
                                                      g <- get 
                                                      put $ continue g

-- Move key

handleEvent (VtyEvent (V.EvKey (V.KChar 's') [])) = do
                                                      g <- get
                                                      put $ move0 id (subtract 1) g 
handleEvent (VtyEvent (V.EvKey (V.KChar 'w') [])) = do
                                                      g <- get
                                                      put $ move0 id (+ 1) g     
handleEvent (VtyEvent (V.EvKey (V.KDown) [])) = do
                                                      g <- get
                                                      put $ move1 id (subtract 1) g 
handleEvent (VtyEvent (V.EvKey (V.KUp) [])) = do
                                                      g <- get
                                                      put $ move1 id (+ 1) g    

handleEvent (VtyEvent (V.EvKey V.KRight []))      = do
                                                      g <- get
                                                      put $ move1 (+ 1) id g
handleEvent (VtyEvent (V.EvKey (V.KChar 'd') [])) = do
                                                      g <- get
                                                      put $ move0 (+ 1) id g
handleEvent (VtyEvent (V.EvKey V.KLeft []))       = do
                                                      g <- get
                                                      put $ move1 (subtract 1) id g
handleEvent (VtyEvent (V.EvKey (V.KChar 'a') [])) = do
                                                      g <- get
                                                      put $ move0 (subtract 1) id g   


handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
                                                      g <- get 
                                                      put $ shoot0 g

handleEvent (VtyEvent (V.EvKey (V.KChar '.') [])) = do
                                                      g <- get 
                                                      put $ shoot1 g 
                                                                                                                                                                                                       
handleEvent _                                     = do
                                                      g <- get
                                                      put g

-- TODO: Update game state