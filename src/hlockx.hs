
module Main (
              main
            ) where

import Data.Bits ((.|.))
import Data.Char (isControl)

import Control.Monad (when)

import Graphics.X11.Types
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types

import Foreign.Marshal.Alloc

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
processInput pw input Nothing _ = Just input
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

-- Checks if the password can be build of the remaining string
-- so that only the tail of the remaing string appended in the
-- front of the string to check
checkPasswords' :: String -> String -> String -> Maybe String
checkPasswords' pw rem str
	| checkPassword pw str = Nothing
	| rem == "" = Just str
	| otherwise = checkPasswords' pw (safeInit rem) (safeLast rem ++ str)

checkPassword :: String -> String -> Bool
checkPassword pw str
	| encrypt == pw = True
	| otherwise = False
		where encrypt = unsafePerformIO $ crypt str pw

safeNotControl :: String -> Bool
safeNotControl "" = False
safeNotControl (x:xs) = not $ isControl x

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
eventLoop' dpy pw e input = do
	nextEvent dpy e
	et <- get_EventType e
	if et == keyPress then do
		(ksym, str) <- lookupString $ asKeyEvent e
		case processInput pw input ksym str of
			Just a -> eventLoop' dpy pw e a
			Nothing -> return ()
	 else eventLoop' dpy pw e input



main :: IO ()
main = do
	progName <- getProgName
	pw <- getPasswordHash
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

	sync dpy True
	eventLoop dpy pw

	cleanup dpy win


