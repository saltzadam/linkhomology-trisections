{-|
Module      : Classification.Testing.SSSTest
Description : Tests for the SSS classification scheme
Copyright   : (c) Adam Saltz, 2020
License     : GPL-3
Maintainer  : saltz.adam@gmail.com
Stability   : experimental
Portability : POSIX

Tests for the classification of Sarkar-Seed-Szabo configurations.
-}
module Classification.Testing.SSSTest
  where
import Classification.Tester
import Core.Configuration (Configuration)

-- * Test configurations

sss1 :: Configuration
sss1 = fromPic sss1string
sss1string :: [Char]
sss1string =  ".-.\n\
               \| |\n\
               \. .\n\
               \|>|\n\
               \. .\n\
               \|>|\n\
               \. .\n\
               \|>|\n\
               \.-."

sss2 :: Configuration
sss2 = fromPic sss2string
sss2string :: [Char]
sss2string =   ".-.\n\
               \| |\n\
               \.-.\n\
               \ v \n\
               \.-.\n\
               \| |\n\
               \.-.\n\
               \ v \n\
               \.-.\n\
               \| |\n\
               \.-.\n\
               \ ^ \n\
               \.-.\n\
               \| |\n\
               \.-."

-- | Vertical union of 'sss1' and 'sss2'.
sss3 :: Configuration
sss3 = fromPic $ sss1string ++ "\n\n" ++ sss2string
