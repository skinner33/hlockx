{-# LANGUAGE CPP #-}

module Main ( main
            ) where

import Hlockx

import Paths_hlockx (version)
import Data.Version (showVersion)

import Foreign.C.Types (CUInt)

import System.Environment
import System.Exit
import System.IO
import System.Console.GetOpt

data Options = Options { optSLock   :: Bool
                       , optKiosk   :: Bool
                       , optTimeout :: CUInt  }

startOptions :: Options
#ifdef DEF_SLOCK
startOptions = Options  { optSLock = True
#else
startOptions = Options  { optSLock = False
#endif
                        , optTimeout = 2
                        , optKiosk   = False
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
   [ Option "v" ["version"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (prg ++ " version " ++ showVersion version)
                exitSuccess))
        "Print version"
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitSuccess))
        "Show help"
    , Option "t" ["timeout"]
        (ReqArg
            (\arg opt -> return opt { optTimeout = read arg }) "Seconds")
        "Set DPMS timeout (default: 2s)"
    , Option "k" ["kiosk"]
        (NoArg
            (\opt -> return opt { optKiosk = True}))
        "Sets kiosk mode (no black screen)"
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
	hlockx (optSLock opts) (optTimeout opts) (optKiosk opts)
