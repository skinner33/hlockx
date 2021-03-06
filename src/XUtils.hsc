{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module XUtils ( cleanup
              , createCursor
              , get_VisibilityEvent
              , grabInputs
              , makeWin
              ) where

import Control.Monad (when)

import Foreign
import Foreign.C.Types

import Graphics.X11.Types
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import System.Exit

import System.IO

import System.Posix.Unistd


cleanup :: Display -> Window -> IO ()
cleanup dpy win = do
	destroyWindow dpy win
	closeDisplay dpy

createCursor :: Display -> ScreenNumber -> Window -> IO ()
createCursor dpy scrNr win = do
	pmap <- createPixmap dpy win 1 1 1
	(black, _) <- allocNamedColor dpy (defaultColormap dpy scrNr) "black"
	invCursor <- createPixmapCursor dpy pmap pmap black black 1 1
	defineCursor dpy win invCursor
	freeCursor dpy invCursor
	freePixmap dpy pmap

grabInputs :: Display -> Window -> Window -> String -> IO ()
grabInputs dpy root win progName = do
	tryGrab progName "pointer" dpy win (grabPointer dpy root False
	    (buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask)
	    grabModeAsync grabModeAsync none none currentTime)
	tryGrab progName "keyboard" dpy win (grabKeyboard dpy root True
	    grabModeAsync grabModeAsync currentTime)

(?) :: Bool -> (t, t) -> t
a ? (b, c) = if a then b else c

makeWin :: Display -> ScreenNumber -> Window -> Bool -> IO Window
makeWin dpy scrNr rw kiosk = do
	let scr = screenOfDisplay dpy scrNr
	    bp = blackPixel dpy scrNr
	    visual   = defaultVisualOfScreen scr
	    depth    = defaultDepthOfScreen scr
	    attrmask = cWOverrideRedirect .|.  cWBackPixel
	    w = kiosk ? (1, fromIntegral $ displayWidth dpy scrNr)
	    h = kiosk ? (1, fromIntegral $ displayHeight dpy scrNr)
	allocaSetWindowAttributes $
		\attributes -> do
			set_override_redirect attributes True
			set_background_pixel attributes bp
			createWindow dpy rw 0 0 w h 0 depth copyFromParent
			             visual attrmask attributes

tryGrab :: String -> String -> Display -> Window -> IO GrabStatus -> IO ()
tryGrab x y dpy win func = tryGrab' x y dpy win func 1000

tryGrab' :: String -> String -> Display -> Window -> IO GrabStatus -> Integer
            -> IO ()
tryGrab' progName typ dpy win _ 0 = do
		hPutStrLn stderr (progName ++ ": couldn't grab " ++ typ)
		cleanup dpy win
		exitFailure
tryGrab' x y dpy win func rty = do
	retK <- func
	when (retK /= grabSuccess) $ do
		usleep 1000
		tryGrab' x y dpy win func (rty - 1)

#include "X11/Xlib.h"

type XVisibilityEvent =	CInt -- state

peekXVisibilityEvent :: Ptr XVisibilityEvent -> IO XVisibilityEvent
peekXVisibilityEvent p = do
        state <- #{peek XVisibilityEvent,state} p
        return state

get_VisibilityEvent :: XEventPtr -> IO XVisibilityEvent
get_VisibilityEvent p = peekXVisibilityEvent (castPtr p)
