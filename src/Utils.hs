
module Utils ( limitInput
             , mapf
             , safeInit
             , safeNotControl
             , safeLast
             , safeTail
             ) where

import Data.Char (isControl)

-- limit length of input
limitInput :: String -> String
limitInput input
	| length input == 255 = tail input
	| otherwise = input

mapf :: [a -> b] -> a -> [b]
mapf [] _     = []
mapf (f:fs) x = f x : mapf fs x

safeInit :: String -> String
safeInit "" = ""
safeInit str = init str

safeNotControl :: String -> Bool
safeNotControl "" = False
safeNotControl (x:_) = not $ isControl x

safeLast :: String -> String
safeLast "" = ""
safeLast str = [last str]

safeTail :: String -> String
safeTail "" = ""
safeTail str = tail str
