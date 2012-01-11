{-# LANGUAGE CPP #-}

module PwUtils ( generateOnePassword
               , generateMulPasswords
#ifndef USE_PAM
               , getPasswordHash
               , checkHash
#else
               , checkPAM
#endif
               , auther
               ) where

import Utils

import System.IO.Unsafe

#ifdef USE_PAM
import System.Posix.PAM
#else

import System.Posix.User

import System.Unix.Crypt
import System.Unix.Shadow
#endif


auther :: String -> (String -> [String]) -> (String -> String -> Bool) -> Maybe String -> Bool
auther _ _ _ Nothing = False
auther token generator checker (Just password) =
	any check (generator password)
	where check = checker token

#ifndef USE_PAM

getPasswordHash :: IO String
getPasswordHash = do
	username <- getLoginName
	entry <- getSUserEntryForName username
	return (sUserPassword entry)

checkHash :: String -> String -> Bool
checkHash hashPw password =
	let result = unsafePerformIO $ crypt password hashPw in
		(result == hashPw)
#else

checkPAM :: String -> String -> Bool
checkPAM username password =
	let result = unsafePerformIO $ authenticate "login" username password in
		case result of
			Left _  -> False
			Right _ -> True
#endif


-- Takes a string and generates all substrings including the
-- last character of the string
generateMulPasswords :: String -> [String]
generateMulPasswords input
	| input == "" = [""]
	| length input == 1 = [input]
	| otherwise = generateMulPasswords (safeTail input) ++ [input]

generateOnePassword :: String -> [String]
generateOnePassword input = [input]
