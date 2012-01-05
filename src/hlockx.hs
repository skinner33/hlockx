
module Main (
              main
            ) where

import Data.Bits ((.|.))

import Control.Monad (when)

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types

import Foreign.Marshal.Alloc

import System.IO
import System.Environment
import System.Exit
import System.Posix.User
import System.Unix.Crypt
import System.Unix.Shadow

getPasswordHash :: IO String
getPasswordHash = do
	username <- getLoginName
	entry <- getSUserEntryForName username
	return (sUserPassword entry)

makeWin :: Display -> ScreenNumber -> Window -> IO Window
makeWin dpy scrNr rw = do
	let scr = screenOfDisplay dpy scrNr
	    bp = blackPixel dpy scrNr
	    visual   = defaultVisualOfScreen scr
	    depth    = defaultDepthOfScreen scr
	    attrmask = (cWOverrideRedirect .|.  cWBackPixel)
	    w = fromIntegral $ displayWidth dpy scrNr
	    h = fromIntegral $ displayHeight dpy scrNr
	allocaSetWindowAttributes $
		\attributes -> do
			set_override_redirect attributes True
			set_background_pixel attributes bp
			createWindow dpy rw 0 0 w h 0 depth copyFromParent
			             visual attrmask attributes


cleanup :: Display -> Window -> IO ()
cleanup dpy win = do
	destroyWindow dpy win
	closeDisplay dpy


main :: IO ()
main = do
	progName <- getProgName
	dpy <- openDisplay ""
	let srcNr = defaultScreen dpy
	root <- rootWindow dpy srcNr
	win <- makeWin dpy srcNr root
	mapRaised dpy win

	ret <- grabPointer dpy root False (buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask) grabModeAsync grabModeAsync none none currentTime
	when (ret /= grabSuccess) $ do
		hPutStrLn stderr (progName ++ ": couldn't grap pointer")
		cleanup dpy win
		exitFailure

	ret <- grabKeyboard dpy root True grabModeAsync grabModeAsync currentTime
	when (ret /= grabSuccess) $ do
		hPutStrLn stderr (progName ++ ": couldn't grap keyboard")
		cleanup dpy win
		exitFailure

	pw <- getPasswordHash
	input <- getLine
	encrypt <- crypt input pw
	if encrypt == pw then
		putStrLn "Yes"
	 else
		putStrLn "No"

	cleanup dpy win


