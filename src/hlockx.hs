
module Main (
              main
            ) where

import Data.Bits ((.|.))

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types

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



main :: IO ()
main = do
	dpy <- openDisplay ""
	let srcNr = defaultScreen dpy
	root <- rootWindow dpy srcNr
	win <- makeWin dpy srcNr root
	mapRaised dpy win
	ret <- grabPointer dpy win False (buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask) grabModeAsync grabModeAsync none none currentTime

	pw <- getPasswordHash
	input <- getLine
	encrypt <- crypt input pw
	if encrypt == pw then
		putStrLn "Yes"
	 else
		putStrLn "No"

	destroyWindow dpy win
	closeDisplay dpy


