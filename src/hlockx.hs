
module Main (
              main
            ) where

import DPMS

import Data.Bits ((.|.))
import Data.Char (isControl)

import Control.Monad (when, unless)

import Graphics.X11.Types
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Foreign.Marshal.Alloc
import Foreign.Storable

import System.IO
import System.Environment
import System.Exit
import System.Posix.User
import System.Unix.Crypt
import System.Unix.Shadow

import System.IO.Unsafe

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

processInput :: String -> String -> Maybe KeySym -> String -> Maybe String
processInput _ input Nothing _ = Just input
processInput pw input (Just ksym) str
	| ksym `elem` [xK_Return, xK_KP_Enter, xK_Escape] = Just ""
	| ksym == xK_BackSpace = Just $ safeInit input
	| xK_KP_0 <= ksym || ksym <= xK_KP_9 = checkPasswords pw $ input ++ str
	| isFunctionKey ksym || isKeypadKey ksym || isMiscFunctionKey ksym = Just input
	| isPFKey ksym || isPrivateKeypadKey ksym = Just input
	| safeNotControl str = checkPasswords pw $ input ++ str
	| otherwise = Just input

-- Takes a password and a string and checks if
-- the password is equal to a substring of the string
-- from the tail of the string starting
checkPasswords :: String -> String -> Maybe String
checkPasswords pw str = checkPasswords' pw (safeInit str) (safeLast str)

-- Checks if the password can be build of the pool string
-- so that only the tail of the pool string appended in the
-- front of the string to check
checkPasswords' :: String -> String -> String -> Maybe String
checkPasswords' pw pool str
	| checkPassword pw str = Nothing
	| pool == "" = Just str
	| otherwise = checkPasswords' pw (safeInit pool) (safeLast pool ++ str)

checkPassword :: String -> String -> Bool
checkPassword pw str
	| encrypt == pw = True
	| otherwise = False
		where encrypt = unsafePerformIO $ crypt str pw

safeNotControl :: String -> Bool
safeNotControl "" = False
safeNotControl (x:_) = not $ isControl x

safeInit :: String -> String
safeInit "" = ""
safeInit str = init str

safeLast :: String -> String
safeLast "" = ""
safeLast str = [last str]

eventLoop :: Display -> String -> IO ()
eventLoop dpy pw =
	allocaXEvent $ \e -> eventLoop' dpy pw e ""

eventLoop' :: Display -> String -> XEventPtr -> String -> IO ()
eventLoop' dpy pw e inp = do
	let input = limitInput inp
	nextEvent dpy e
	et <- get_EventType e
	if et == keyPress then do
		(ksym, str) <- lookupString $ asKeyEvent e
		case processInput pw input ksym str of
			Just a -> eventLoop' dpy pw e a
			Nothing -> return ()
	 else eventLoop' dpy pw e input

-- limit length of input
limitInput :: String -> String
limitInput input
	| length input == 255 = tail input
	| otherwise = input

createCursor :: Display -> ScreenNumber -> Window -> IO ()
createCursor dpy scrNr win = do
	pmap <- createPixmap dpy win 1 1 1
	(black, _) <- allocNamedColor dpy (defaultColormap dpy scrNr) "black"
	invCursor <- createPixmapCursor dpy pmap pmap black black 1 1
	defineCursor dpy win invCursor
	freeCursor dpy invCursor
	freePixmap dpy pmap

main :: IO ()
main = do
	progName <- getProgName
	pw <- getPasswordHash
	dpy <- openDisplay ""
	let scrNr = defaultScreen dpy
	root <- rootWindow dpy scrNr
	win <- makeWin dpy scrNr root

	createCursor dpy scrNr win

	retP <- grabPointer dpy root False (buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask) grabModeAsync grabModeAsync none none currentTime
	when (retP /= grabSuccess) $ do
		hPutStrLn stderr (progName ++ ": couldn't grap pointer")
		cleanup dpy win
		exitFailure

	retK <- grabKeyboard dpy root True grabModeAsync grabModeAsync currentTime
	when (retK /= grabSuccess) $ do
		hPutStrLn stderr (progName ++ ": couldn't grap keyboard")
		cleanup dpy win
		exitFailure

	-- check if DPMS was enabled before enabling it
	(standby, suspend, off, wasEnabled) <- alloca $ \pOff ->
		alloca $ \pSuspend ->
		alloca $ \pStandby ->
		alloca $ \dpmsWasActive ->
		alloca $ \dpmsPowerLevel -> do
			_ <- dPMSInfo dpy dpmsPowerLevel dpmsWasActive
			_ <- dPMSGetTimeouts dpy pStandby pSuspend pOff
			oldStandby <- peek pStandby
			oldSuspend <- peek pSuspend
			oldOff     <- peek pOff
			oldStatus  <- peek dpmsWasActive
			return (oldStandby, oldSuspend, oldOff, oldStatus)

	_ <- dPMSEnable dpy
	_ <- dPMSSetTimeouts dpy 0 0 2

	_ <- mapRaised dpy win

	sync dpy True
	eventLoop dpy pw

	-- restore previous DPMS settings
	_ <- dPMSSetTimeouts dpy standby suspend off
	unless wasEnabled $ do _ <- dPMSDisable dpy; return ()

	ungrabPointer dpy currentTime

	cleanup dpy win


