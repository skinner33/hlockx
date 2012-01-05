
module Main (
              main
            ) where

import Graphics.X11.Xlib

import System.Posix.User
import System.Unix.Crypt
import System.Unix.Shadow

getPasswordHash :: IO String
getPasswordHash = do
	username <- getLoginName
	entry <- getSUserEntryForName username
	return (sUserPassword entry)

main :: IO ()
main = do
	dpy <- openDisplay ""
	pw <- getPasswordHash
	input <- getLine
	encrypt <- crypt input pw
	if encrypt == pw then
		putStrLn "Yes"
	 else
		putStrLn "No"
	closeDisplay dpy


