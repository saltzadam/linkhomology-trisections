{-|
Module      : Classification.Testing.NewETests
Description : Test for the Type E classification scheme
Copyright   : (c) Adam Saltz, 2020
License     : GPL-3
Maintainer  : saltz.adam@gmail.com
Stability   : experimental
Portability : POSIX

Tests for the Type E classification scheme.  TODO: put into a proper test suite.
-}
module Classification.Testing.NewETests
  where
import Classification.NewE
import Core.Configuration
import Classification.Tester
import Data.Maybe (fromJust)

-- * Example two-dimensional configurations
szabo1 :: TwoDConf
szabo1 = (fromJust . confToTwoDConf . fromPic)  szabo1String
szabo1' :: TwoDConf
szabo1' = (fromJust . confToTwoDConf . fromPic)  szabo1String'
szabo2 :: TwoDConf
szabo2 = (fromJust . confToTwoDConf . fromPic)  szabo2String
szabo2' :: TwoDConf
szabo2' = (fromJust . confToTwoDConf . fromPic)  szabo2String'
szabo2'' :: TwoDConf
szabo2'' = (fromJust . confToTwoDConf . fromPic)  szabo2String''
szabo3 :: TwoDConf
szabo3 = (fromJust . confToTwoDConf . fromPic)  szabo3String
szabo3' :: TwoDConf
szabo3' = (fromJust . confToTwoDConf . fromPic)  szabo3String'
szabo4 :: TwoDConf
szabo4 = (fromJust . confToTwoDConf . fromPic)  szabo4String
szabo4' :: TwoDConf
szabo4' = (fromJust . confToTwoDConf . fromPic)  szabo4String'
szabo4'' :: TwoDConf
szabo4'' = (fromJust . confToTwoDConf . fromPic)  szabo4String''
szabo4''' :: TwoDConf
szabo4''' = (fromJust . confToTwoDConf . fromPic)  szabo4String'''
szabo5 :: TwoDConf
szabo5 = (fromJust . confToTwoDConf . fromPic)  szabo5String
szabo6 :: TwoDConf
szabo6 = (fromJust . confToTwoDConf . fromPic)  szabo6String
szabo6' :: TwoDConf
szabo6' = (fromJust . confToTwoDConf . fromPic)  szabo6String'
szabo7 :: TwoDConf
szabo7 = (fromJust . confToTwoDConf . fromPic)  szabo7String
szabo7' :: TwoDConf
szabo7' = (fromJust . confToTwoDConf . fromPic)  szabo7String'

-- ** Reverse of the above configurations
szabo1r :: TwoDConf
szabo1r = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo1String
szabo1r' :: TwoDConf
szabo1r' = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo1String'
szabo2r :: TwoDConf
szabo2r = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo2String
szabo2r' :: TwoDConf
szabo2r' = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo2String'
szabo2r'' :: TwoDConf
szabo2r'' = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo2String''
szabo3r :: TwoDConf
szabo3r = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo3String
szabo3r' :: TwoDConf
szabo3r' = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo3String'
szabo4r :: TwoDConf
szabo4r = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo4String
szabo4r' :: TwoDConf
szabo4r' = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo4String'
szabo4r'' :: TwoDConf
szabo4r'' = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo4String''
szabo4r''' :: TwoDConf
szabo4r''' = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo4String'''
szabo5r :: TwoDConf
szabo5r = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo5String
szabo6r :: TwoDConf
szabo6r = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo6String
szabo6r' :: TwoDConf
szabo6r' = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo6String'
szabo7r :: TwoDConf
szabo7r = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo7String
szabo7r' :: TwoDConf
szabo7r' = (fromJust .  confToTwoDConf . reverseConfig . fromPic)  szabo7String'

-- * The tests
testNewE :: TwoDConf -> [Bool]
testNewE c = ($ c) <$> [isType23, isType4, isType5, isType67away, isType67towards]

newTests :: [Bool]
newTests = [testNewE szabo1  == [False, False,False,False,False],
            testNewE szabo1' == [False,False,False,False,False],
            testNewE szabo2  == [True,False,False,False,False],
            testNewE szabo2'  == [True,False,False,False,False],
            testNewE szabo2''  == [True,False,False,False,False],
            testNewE szabo3 == [True,False,False,False,False],
            testNewE szabo3' == [True,False,False,False,False],
            testNewE szabo4 == [False,True,False,False,False], 
            testNewE szabo4' == [False,True,False,False,False], 
            testNewE szabo4'' == [False,True,False,False,False], 
            testNewE szabo4''' == [False,True,False,False,False],
            testNewE szabo5 == [False,False,True,False,False],
            testNewE szabo6 == [False,False,False,True,False] || (testNewE szabo6 )== [False,False,False,False,True],
            testNewE szabo6' == [False,False,False,True,False] || (testNewE szabo6') == [False,False,False,False,True],
            testNewE szabo7 == [False,False,False,True,False] || (testNewE szabo7 )== [False,False,False,False,True],
            testNewE szabo7' == [False,False,False,True,False] || (testNewE szabo7') == [False,False,False,False,True]]
newTestsReversed :: [Bool]
newTestsReversed = [testNewE szabo1r  == [False, False,False,False,False],
            testNewE szabo1r' == [False,False,False,False,False],
            testNewE szabo2r  == [True,False,False,False,False],
            testNewE szabo2r'  == [True,False,False,False,False],
            testNewE szabo2r''  == [True,False,False,False,False],
            testNewE szabo3r == [True,False,False,False,False],
            testNewE szabo3r' == [True,False,False,False,False],
            testNewE szabo4r == [False,True,False,False,False], 
            testNewE szabo4r' == [False,True,False,False,False], 
            testNewE szabo4r'' == [False,True,False,False,False], 
            testNewE szabo4r''' == [False,True,False,False,False],
            testNewE szabo5r == [False,False,True,False,False],
            testNewE szabo6r == [False,False,False,True,False] || (testNewE szabo6r )== [False,False,False,False,True],
            testNewE szabo6r' == [False,False,False,True,False] || (testNewE szabo6r') == [False,False,False,False,True],
            testNewE szabo7r == [False,False,False,True,False] || (testNewE szabo7r )== [False,False,False,False,True],
            testNewE szabo7r' == [False,False,False,True,False] || (testNewE szabo7r') == [False,False,False,False,True]]

-- | A two-dimensional configurations of type 2 - 7 is "ruined" if you reverse one of the decorations -- it won't be of that type any more.  This tests that the "ruined" decorations are properly classified as such. 
newTestsRuined :: [Bool]
newTestsRuined = let allFalse = [False,False,False,False,False]
  in
    fmap ((== allFalse)  . (testNewE . ruin)) [szabo1,szabo2,szabo2',szabo2'',szabo3,szabo3',szabo4,szabo4',szabo4'',szabo4''',szabo5,szabo6,szabo6',szabo7,szabo7']

newTestsReversedRuined :: [Bool]
newTestsReversedRuined = let allFalse = [False,False,False,False,False]
  in
    fmap ((== allFalse)  . (testNewE . ruin)) [szabo1r,szabo2r,szabo2r',szabo2r'',szabo3r,szabo3r',szabo4r,szabo4r',szabo4r'',szabo4r''',szabo5r,szabo6r,szabo6r',szabo7r,szabo7r']


-- * String pictures for the test configurations.
szabo1String :: String
szabo1String  =  ".-.-. .-.-.\n\
                  \|   |>|   |\n\
                  \.   . .   .\n\
                  \|   |>|   |\n\
                  \.-.-. .-.-."
szabo1String' :: String
szabo1String' = ".-.-.-.-.-.\n\
                \|         |\n\
                \. .-.-.-. .\n\
                \|<|     |>|\n\
                \. .-.-.-. .\n\
                \|         |\n\
                \.-.-.-.-.-."

szabo2String :: String
szabo2String = ".-. .-.\n\
               \| |<| |\n\
               \.-. .-.\n\
               \ ^     \n\
               \.-.    \n\
               \| |    \n\
               \.-."

szabo2String' :: String
szabo2String' = ".-.-.-.-.-.\n\
                \|         |\n\
                \. .-. .-. .\n\
                \|<| | | |>|\n\
                \. .-. .-. .\n\
                \|         |\n\
                \.-.-.-.-.-."

szabo2String'' :: String
szabo2String''= ".-.-.-.-.-.\n\
                \|         |\n\
                \. .-. .-. .\n\
                \| | |>| |<|\n\
                \. .-. .-. .\n\
                \|         |\n\
                \.-.-.-.-.-."

szabo3String :: String
szabo3String =  ".-.-.-.-.-.-.-.-.    \n\
                \|      ^        |    \n\
                \.     .-.       .    \n\
                \|     | |       |    \n\
                \.     .-.       .    \n\
                \|               |    \n\
                \.               . .-.\n\
                \|               |<| |\n\
                \.-.-.-.-.-.-.-.-. .-."

szabo3String' :: String
szabo3String' = ".-.-.-.-.-.-.-.-.\n\
               \|               |\n\
               \. .-.-.-.-.-.-. .\n\
               \| |           | |\n\
               \. . .-.-.-.-. . .\n\
               \| |<|       | |<|\n\
               \. . .-.-.-.-. . .\n\
               \| |           | |\n\
               \. .-.-.-.-.-.-. .\n\
               \|               |\n\
               \.-.-.-.-.-.-.-.-."
 
szabo4String :: String
szabo4String =  ".-.\n\
                \|<|\n\
                \. .\n\
                \|>|\n\
                \. .\n\
                \| |\n\
                \.-."

szabo4String' :: String
szabo4String' = ".-.\n\
                \|>|\n\
                \. .\n\
                \|<|\n\
                \. .\n\
                \| |\n\
                \.-."

szabo4String'' :: String
szabo4String'' = ".-. .-.\n\
                 \| |>| |\n\
                 \. .-. .\n\
                 \|     |\n\
                 \. .-. .\n\
                 \| |<| |\n\
                 \.-. .-."

szabo4String''' :: String
szabo4String''' = ".-. .-.\n\
                  \| |<| |\n\
                  \. .-. .\n\
                  \|     |\n\
                  \. .-. .\n\
                  \| |>| |\n\
                  \.-. .-."

szabo5String :: String
szabo5String =     "  .-.  \n\
                   \  |>|  \n\
                   \.-. .-.\n\
                   \|     |\n\
                   \. .-. .\n\
                   \| |>| |\n\
                   \.-. .-."

szabo6String :: String
szabo6String = ".-.\n\
                \| |\n\
                \.-.\n\
                \ ^ \n\
                \.-.\n\
                \|>|\n\
                \. .\n\
                \| |\n\
                \.-."

szabo6String' :: String
szabo6String' = ".-.-.-.-.\n\
                \|  v    |\n\
                \. .-.   .\n\
                \| | |   |\n\
                \. .-.   .\n\
                \|       |\n\
                \.   .-. .\n\
                \|   |<| |\n\
                \.-.-. .-."


szabo7String :: String
szabo7String =  ".-.-.-.\n\
                \|  v  |\n\
                \. .-. .\n\
                \| | | |\n\
                \. .-. .\n\
                \|     |\n\
                \. .-.-.\n\
                \|>|    \n\
                \.-.    "

-- not 100% on orientation of bottom arrow
szabo7String' :: String
szabo7String' = ".-.\n\
                \| |\n\
                \.-.\n\
                \ ^ \n\
                \.-.-.-.\n\
                \|     |\n\
                \. .-. .\n\
                \| |<| |\n\
                \.-. .-."

szabo8String :: String
szabo8String= ".-.-.-.\n\
              \|     |\n\
              \. .-.-.\n\
              \|>|  v \n\
              \. .-.-.\n\
              \|     |\n\
              \.-.-.-."

