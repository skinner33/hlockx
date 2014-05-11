{-# LANGUAGE CPP #-}

module Hlockx ( hlockx
              ) where

import DPMS
import PwUtils
import Utils
import XUtils

import Control.Monad (unless, when)

import Foreign.C.Types (CUInt)

import Graphics.X11.Types
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import System.Environment

#ifdef USE_PAM
import System.Posix.User
#endif


hlockx :: Bool -> CUInt -> Bool -> IO ()
hlockx slock timeout kiosk = do
	progName <- getProgName

#ifndef USE_PAM
	token <- getPasswordHash
	let check = checkHash
#else
	uid <- getRealUserID
	userEntry <- getUserEntryForID uid
	let token = userName userEntry
	let check = checkPAM
#endif

	dpy <- openDisplay ""
	let scrNr = defaultScreen dpy
	root <- rootWindow dpy scrNr
	win <- makeWin dpy scrNr root kiosk
	selectInput dpy win visibilityChangeMask
	createCursor dpy scrNr win
	grabInputs dpy root win progName
	_ <- mapRaised dpy win
	sync dpy True

	-- check if DPMS was enabled before enabling it
	(standby, suspend, off, wasEnabled) <- getCurrentDPMSStatus dpy

	_ <- if kiosk
		then dPMSDisable dpy
		else do
			_ <- dPMSEnable dpy
			_ <- dPMSSetTimeouts dpy 0 0 timeout
			dPMSForceLevel dpy dPMSModeOff


	if slock then
		eventLoop dpy win (auther token generateOnePassword check) processInputSLock
	 else
		eventLoop dpy win (auther token generateMulPasswords check) processInputLockX

	-- restore previous DPMS settings
	_ <- dPMSSetTimeouts dpy standby suspend off
	unless wasEnabled $ do _ <- dPMSDisable dpy; return ()

	ungrabPointer dpy currentTime
	ungrabKeyboard dpy currentTime

	cleanup dpy win

-- check after every character input
processInputLockX :: String -> (Maybe KeySym, String) -> (String, Maybe String)
processInputLockX input (Nothing, _) = (input, Nothing)
processInputLockX input (Just ksym, str)
	| ksym `elem` [xK_Return, xK_KP_Enter, xK_Escape] = ("", Nothing)
	| ksym == xK_BackSpace = (safeInit input, Nothing)
	| xK_KP_0 <= ksym || ksym <= xK_KP_9 = let a = input ++ str in
		(a, Just a)
	| or $ mapf [isFunctionKey, isKeypadKey, isMiscFunctionKey, isPFKey,
	  isPrivateKeypadKey] ksym = (input, Nothing)
	| safeNotControl str = let a = input ++ str in
		(a, Just a)
	| otherwise = (input, Nothing)

-- check after pressing enter
processInputSLock :: String -> (Maybe KeySym, String) -> (String, Maybe String)
processInputSLock input (Nothing, _) = (input, Nothing)
processInputSLock input (Just ksym, str)
	| ksym == xK_Escape = ("", Nothing)
	| ksym `elem` [xK_Return, xK_KP_Enter] = ("", Just input)
	| ksym == xK_BackSpace = (safeInit input, Nothing)
	| xK_KP_0 <= ksym || ksym <= xK_KP_9 = (input ++ str, Nothing)
	| or $ mapf [isFunctionKey, isKeypadKey, isMiscFunctionKey, isPFKey,
	  isPrivateKeypadKey] ksym = (input, Nothing)
	| safeNotControl str = (input ++ str, Nothing)
	| otherwise = (input, Nothing)

eventLoop :: Display -> Window -> (Maybe String -> Bool) -> (String -> (Maybe KeySym, String) -> (String, Maybe String)) -> IO ()
eventLoop dpy win auth process =
	allocaXEvent $ \e -> eventLoop' dpy win auth process e ""

eventLoop' :: Display -> Window -> (Maybe String -> Bool) -> (String -> (Maybe KeySym, String) -> (String, Maybe String)) -> XEventPtr -> String -> IO ()
eventLoop' dpy win auth process e input' = do
	nextEvent dpy e
	et <- get_EventType e
	selectAction et input'
	where
	selectAction et input
		| et == visibilityNotify = do
			vis <- get_VisibilityEvent e
			when (vis /= visibilityUnobscured) $ do
				_ <- mapRaised dpy win
				return ()
			eventLoop' dpy win auth process e input
		| et == keyPress = do
			(ksym, str) <- lookupString $ asKeyEvent e
			let (a, check) = process input (ksym, str) in
				(unless (auth check) $
					eventLoop' dpy win auth process e $ limitInput a)
		| otherwise = eventLoop' dpy win auth process e input
