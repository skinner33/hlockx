module Hlockx ( hlockx
              ) where

import DPMS
import PwUtils
import Utils
import XUtils

import Control.Monad (unless)

import Foreign.C.Types (CUInt)

import Graphics.X11.Types
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import System.Environment


hlockx :: Bool -> CUInt -> IO ()
hlockx slock timeout = do
	progName <- getProgName

	pw <- getPasswordHash

	dpy <- openDisplay ""
	let scrNr = defaultScreen dpy
	root <- rootWindow dpy scrNr
	win <- makeWin dpy scrNr root
	createCursor dpy scrNr win
	grabInputs dpy root win progName
	_ <- mapRaised dpy win
	sync dpy True

	-- check if DPMS was enabled before enabling it
	(standby, suspend, off, wasEnabled) <- getCurrentDPMSStatus dpy
	_ <- dPMSEnable dpy
	_ <- dPMSSetTimeouts dpy 0 0 timeout
	_ <- dPMSForceLevel dpy dPMSModeOff


	if slock then
		eventLoop dpy pw processInputSLock
	 else
		eventLoop dpy pw processInputLockX

	-- restore previous DPMS settings
	_ <- dPMSSetTimeouts dpy standby suspend off
	unless wasEnabled $ do _ <- dPMSDisable dpy; return ()

	ungrabPointer dpy currentTime
	ungrabKeyboard dpy currentTime

	cleanup dpy win

-- check after every character input
processInputLockX :: String -> String -> Maybe KeySym -> String -> Maybe String
processInputLockX _ input Nothing _ = Just input
processInputLockX pw input (Just ksym) str
	| ksym `elem` [xK_Return, xK_KP_Enter, xK_Escape] = Just ""
	| ksym == xK_BackSpace = Just $ safeInit input
	| xK_KP_0 <= ksym || ksym <= xK_KP_9 = checkPasswords pw $ input ++ str
	| or $ mapf [isFunctionKey, isKeypadKey, isMiscFunctionKey, isPFKey,
	  isPrivateKeypadKey] ksym = Just input
	| safeNotControl str = checkPasswords pw $ input ++ str
	| otherwise = Just input

-- check after pressing enter
processInputSLock :: String -> String -> Maybe KeySym -> String -> Maybe String
processInputSLock _ input Nothing _ = Just input
processInputSLock pw input (Just ksym) str
	| ksym == xK_Escape = Just ""
	| ksym `elem` [xK_Return, xK_KP_Enter] = checkSinglePassword pw input
	| ksym == xK_BackSpace = Just $ safeInit input
	| xK_KP_0 <= ksym || ksym <= xK_KP_9 = Just $ input ++ str
	| or $ mapf [isFunctionKey, isKeypadKey, isMiscFunctionKey, isPFKey,
	  isPrivateKeypadKey] ksym = Just input
	| safeNotControl str = Just $ input ++ str
	| otherwise = Just input

eventLoop :: Display -> String -> (String -> String -> Maybe KeySym -> String -> Maybe String) -> IO ()
eventLoop dpy pw process =
	allocaXEvent $ \e -> eventLoop' dpy pw process e ""

eventLoop' :: Display -> String -> (String -> String -> Maybe KeySym -> String -> Maybe String) -> XEventPtr -> String -> IO ()
eventLoop' dpy pw process e inp = do
	let input = limitInput inp
	nextEvent dpy e
	et <- get_EventType e
	if et == keyPress then do
		(ksym, str) <- lookupString $ asKeyEvent e
		case process pw input ksym str of
			Just a -> eventLoop' dpy pw process e a
			Nothing -> return ()
	 else eventLoop' dpy pw process e input

