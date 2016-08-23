-- reactimate loop funktioniert nicht zusammn mit GLUT --
-- daher muss reactInit + react verwendet werden --
-- Grund: reactimate stellt mainLoop dar -> mainLoop muss in GLUT aber der OpenGL loop sein --
-- Quelle: https://hackage.haskell.org/package/Yampa-0.10.5/docs/FRP-Yampa.html#t:https://hackage.haskell.org/package/Yampa-0.10.5/docs/FRP-Yampa.html#v:reactimate --

---------------------------------------------------------------------------

-- Code nach Quelle: http://code.haskell.org/frag/src/Main.hs --

---------------------------------------------------------------------------

{-# LANGUAGE Arrows #-}

module Main where

import Model
import Game
import VTGraphics
import Graphics.UI.GLUT hiding (position, Position)
import Data.IORef
import FRP.Yampa
import FRP.Yampa.Utilities
import Control.Arrow
import Data.Time.Clock
import Data.Maybe
    
-- set up OpenGL and start program
main :: IO ()
main = do
    (_progname, _args) <- getArgsAndInitialize
    _window <- createAWindow "OpenGL Test"
    mainLoop
    
-- create window with specified attributes and set up game
createAWindow :: String -> IO ()
createAWindow title = do
    -- set up and create window
    initialWindowSize $= (Size 640 480)
    createWindow title
    -- set up IORefs
    t <- getCurrentTime
    timeRef <- newIORef t
    -- saves input
    actionRef <- newIORef AccNone
    -- set up ReactHandle
    rh <- reactInit (initr) (actuate) (mainGameSF initGameState)
    -- set up Callbacks
    displayCallback $= display
    idleCallback $=  Just (idle timeRef actionRef rh)
    reshapeCallback $= Just resizeWindow
    keyboardMouseCallback $= Just (keyboardMouse actionRef)
    return ()


----------------- reactimate functions     ------------------

-- init react
initr :: IO Acceleration
initr = return zeroVector

--actuate react
actuate :: ReactHandle Acceleration GameState -> Bool -> GameState -> IO Bool
actuate _ _ gameState = do 
    renderScene gameState
    return False
    

----------------- openGL callbacks ---------------------------

display :: IO ()
display = do
    return ()
    
-- callback which is called, if openGL is in idle state.
-- main signal function is called from this callback and game logic goes one tick further.
idle :: IORef UTCTime -> IORef Action -> ReactHandle Acceleration GameState -> IO ()
idle timeRef inputRef rh = do
    now <- getCurrentTime
    lastTime <- readIORef timeRef
    writeIORef timeRef now
    input <- readIORef inputRef
    let dt = now `diffUTCTime` lastTime
    react rh (realToFrac dt, Just (convAcc input))
    return ()
    
-- callback which is called, if window gets resized. Handles the resizing process.
resizeWindow :: Size -> IO ()
resizeWindow size = do
    -- TODO keep aspect ratio
    -- viewport $= (Graphics.UI.GLUT.Position 0 0, size)
    return ()

--Wir brauchen hier das GameObject, wenn die Beschleunigung auf der Rotation basieren soll!    

-- callback for handling keyboardMouse and mouse input
keyboardMouse :: IORef Action -> KeyboardMouseCallback
keyboardMouse inputRef (SpecialKey KeyLeft) Down _ _ = inputRef $= AccLeft
keyboardMouse inputRef (SpecialKey KeyRight) Down _ _ = inputRef $= AccRight
keyboardMouse inputRef (SpecialKey KeyUp) Down _ _ = inputRef $= AccUp
keyboardMouse inputRef (SpecialKey KeyDown) Down _ _ = inputRef $= AccDown
keyboardMouse inputRef _ Up _ _ = inputRef $= AccNone
keyboardMouse _ _ _ _ _ = return ()

-- helper for converting input to acceleration
convAcc :: Action -> Acceleration
convAcc AccUp = Vector 0 1
convAcc AccDown = Vector 0 (-1)
convAcc AccLeft = Vector (-1) 0
convAcc AccRight = Vector 1 0
convAcc AccNone = Vector 0 0