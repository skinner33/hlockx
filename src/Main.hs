{-# LANGUAGE CPP #-}

module Main ( main
            ) where

import Hlockx

import System.Environment
import System.Exit
import System.IO
import System.Console.GetOpt

data Options = Options { optSLock :: Bool }

startOptions :: Options
#ifdef DEF_SLOCK
startOptions = Options  { optSLock = True }
#else
startOptions = Options  { optSLock = False }
#endif

options :: [ OptDescr (Options -> IO Options) ]
options =
   [ Option "v" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.01"
                exitWith ExitSuccess))
        "Print version"
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    , Option "s" ["slock"]
        (NoArg
            (\opt -> return opt { optSLock = True }))
#ifdef DEF_SLOCK
        "Enable slock mode (default)"
#else
        "Enable slock mode"
#endif
    , Option "l" ["lockx"]
        (NoArg
            (\opt -> return opt { optSLock = False }))
#ifdef DEF_SLOCK
        "Enable lockx mode"
#else
        "Enable lockx mode (default)"
#endif
    ]

main :: IO ()
main = do
	args <- getArgs
	-- Parse options, getting a list of option actions
	let (actions, _, _) = getOpt RequireOrder options args
	opts <- foldl (>>=) (return startOptions) actions
	hlockx (optSLock opts)
