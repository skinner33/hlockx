module PwUtils ( checkPasswords
               , checkSinglePassword
               , getPasswordHash
               ) where

import Utils

import System.IO.Unsafe

import System.Posix.User

import System.Unix.Crypt
import System.Unix.Shadow


getPasswordHash :: IO String
getPasswordHash = do
	username <- getLoginName
	entry <- getSUserEntryForName username
	return (sUserPassword entry)

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

checkSinglePassword :: String -> String -> Maybe String
checkSinglePassword pw input
	| checkPassword pw input = Nothing
	| otherwise = Just ""

checkPassword :: String -> String -> Bool
checkPassword pw str
	| encrypt == pw = True
	| otherwise = False
		where encrypt = unsafePerformIO $ crypt str pw
