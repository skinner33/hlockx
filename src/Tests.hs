
import PwUtils

import Test.QuickCheck
import Text.Printf

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

deepCheck p = quickCheckWith (stdArgs{ maxSuccess = 10000 }) p

-- and add this to the tests list
tests  = [("generateOnePassword/idArray", deepCheck prop_genOneId)
         ,("generateOnePassword/lengthOne", deepCheck prop_genOneLength)
         ,("generateMulPassword/lengthMinOne", deepCheck prop_genMulLength)
         ,("testCheker works", deepCheck prop_testChecker)
         ,("auther returns true only wenn checker does", deepCheck prop_auther)]

pwCheckTest :: String -> String -> Bool
pwCheckTest a b = (a == b)

prop_genOneId :: String -> Bool
prop_genOneId s = generateOnePassword s == [s]

prop_genOneLength :: String -> Bool
prop_genOneLength s = (length $ generateOnePassword s) == 1

prop_genMulLength :: String -> Bool
prop_genMulLength s = (length $ generateMulPasswords s) >= 1

prop_testChecker :: String -> String -> Bool
prop_testChecker x y = (pwCheckTest x y) == (x == y)

prop_auther :: String -> String -> Bool
prop_auther x y = (x == y) == auther x generateOnePassword pwCheckTest (Just y)
