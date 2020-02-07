module Classification.Testing.Test
where
import Core.Configuration
import Classification.Tester
import Classification.Classification
import LinkHomology.Szabo
import Algebra.V

import Data.Set (Set)

basicTests :: IO ()
basicTests = sequence_ [testIsA typeA, testIsB typeB, testIsC typeC, testIsD typeD, testIsE typeE1, testIsE typeE2]

betterBasicTests :: [Set SzabosConfigs]
betterBasicTests = classifyConfiguration <$> [typeA, typeB, typeC, typeD, typeE1, typeE2, tricky]

longTest :: [Labeling -> Set Labeling]
longTest = szaboOneHandleMap <$>[typeA, typeB, typeC, typeD, typeE1, typeE2, tricky]

trickyTest :: IO ()
trickyTest = sequence_ [testIsA tricky, testIsB tricky, testIsC tricky, testIsD tricky, testIsE tricky]

aString :: String
aString =  ".-.-. .-.-.\n\
  \|   |>|   |\n\
  \.   . .   .\n\
  \|   |>|   |\n\
  \.   . .   .\n\
  \|   |>|   |\n\
  \.   . .   .\n\
  \|   | |   |\n\
  \.-.-. .-.-."

notAString :: String
notAString = ".-.-. .-.-.\n\
             \|   |>|   |\n\
             \.   . .   .\n\
             \|   |<|   |\n\
             \.   . .   .\n\
             \|   |>|   |\n\
             \.   . .   .\n\
             \|   | |   |\n\
             \.-.-. .-.-."

typeA :: Configuration
typeA = fromPic aString
 
bString :: String
bString = ".-. .-.\n\
          \| |>| |\n\
          \.-. .-.\n\
          \ ^   v \n\
          \.-. .-.\n\
          \| |<| |\n\
          \.-. .-."

typeB :: Configuration
typeB = fromPic bString

notBString :: String
notBString = ".-. .-.\n\
             \| |>| |\n\
             \.-. .-.\n\
             \ v   v \n\
             \.-. .-.\n\
             \| |<| |\n\
             \.-. .-."

bString' :: String
bString' = ".-. .-.\n\
           \| |<| |\n\
           \.-. .-.\n\
           \ v   ^ \n\
           \.-. .-.\n\
           \| |>| |\n\
           \.-. .-."

typeB' :: Configuration
typeB' = fromPic bString'

notB :: Configuration
notB = fromPic notBString 

cString :: String
cString = ".-.-.-.-.\n\
          \|  v    |\n\
          \. .-.-.-.\n\
          \|>|  v v \n\
          \. .-.-.-.\n\
          \|  ^    |\n\
          \.-.-.-.-."

typeC :: Configuration
typeC = fromPic cString

notCString :: String
notCString = ".-.-.-.-.\n\
             \|  ^    |\n\
             \. .-.-.-.\n\
             \|<|  v v \n\
             \. .-.-.-.\n\
             \|  v    |\n\
             \.-.-.-.-."

notC :: Configuration
notC = fromPic notCString

notC2 :: Configuration
notC2 = fromPic notCString2

notCString2 :: String
notCString2 = ".-.-.-.-.\n\
          \|  v    |\n\
          \. .-.-.-.\n\
          \|>|    \n\
          \. .-.-.-.\n\
          \|  ^    |\n\
          \.-.-.-.-."




dString :: String
dString = ".-.-.-.-.-. .-.\n\
          \|         |>| |\n\
          \. .-.-.-. . .-.\n\
          \|>|     |>|  v \n\
          \. .-.-.-. . .-.\n\
          \|         | | |\n\
          \.-.-.-.-.-. .-.\n\
          \         ^   v \n\
          \        .-.-.-.\n\
          \        |     |\n\
          \        .-.-.-."

typeD :: Configuration
typeD = fromPic dString

notDString :: String
notDString = ".-.-.-.-.-. .-.\n\
             \|         |>| |\n\
             \. .-.-.-. . .-.\n\
             \|<|     |<|  v \n\
             \. .-.-.-. . .-.\n\
             \|         | | |\n\
             \.-.-.-.-.-. .-.\n\
             \         ^   v \n\
             \        .-.-.-.\n\
             \        |     |\n\
             \        .-.-.-."

notDString2 :: String
notDString2 = ".-.-.-.-.-. .-.\n\
              \|         |>| |\n\
              \. .-.-.-. . .-.\n\
              \|>|     |>|  ^ \n\
              \. .-.-.-. . .-.\n\
              \|         | | |\n\
              \.-.-.-.-.-. .-.\n\
              \         ^   v \n\
              \        .-.-.-.\n\
              \        |     |\n\
              \        .-.-.-."

notD1 :: Configuration
notD1 = fromPic notDString
notD2 :: Configuration
notD2 = fromPic notDString2

typeE1 :: Configuration
typeE1 = fromPic eString1
typeE2 :: Configuration
typeE2 = fromPic eString2

eString1 :: String
eString1 = "    .-.-.-.-.-.-.-.-.    \n\
           \    |      ^   ^    |    \n\
           \.-. .     .-. .-.   .-.  \n\
           \| |>|     | | | |    v|  \n\
           \.-. .     .-. .-.   .-.  \n\
           \    |               |    \n\
           \.-. .               .    \n\
           \| |>|               |    \n\
           \.-. .-.             . .-.\n\
           \     v|             |<| |\n\
           \    .-.   .-.       . .-.\n\
           \    |     | |       |    \n\
           \    .     .-. .-.   .    \n\
           \    |      v  |>|   |    \n\
           \    .-. .-.-.-. .-.-.    \n\
           \      |<|                \n\
           \      .-.                "

eString2 :: String
eString2 = "    .-.-.-.-.-. .-.-.    \n\
           \    |      v  |>|   |    \n\
           \.-. .     .-. .-.   .    \n\
           \| |<|     | |       |    \n\
           \.-. .     .-.   .-. .    \n\
           \    |           | |<|    \n\
           \.-. .           .-. .    \n\
           \| |<|               |    \n\
           \.-. .               . .-.\n\
           \    |               |>| |\n\
           \  .-.               . .-.\n\
           \  |v                |    \n\
           \  .-.         .-.   .    \n\
           \    |         |<|   |    \n\
           \    .-. .-.-.-. .-.-.    \n\
           \      |>|                \n\
           \      .-.                "

simpleEString :: String
simpleEString = ".-.-.-.-.-. .-.-.    \n\
                \|      ^  |<|   |    \n\
                \.     .-. .-.   .    \n\
                \|     | |       |    \n\
                \.     .-.   .-. .    \n\
                \|           | |>|    \n\
                \.           .-. .    \n\
                \|               |    \n\
                \.               . .-.\n\
                \|               |<| |\n\
                \.               . .-.\n\
                \|               |    \n\
                \.         .-.   .    \n\
                \|         |>|   |    \n\
                \.-. .-.-.-. .-.-.    \n\
                \  |<|                \n\
                \  .-.                "

typeESimple :: Configuration
typeESimple = fromPic simpleEString

notEs :: [Configuration]
notEs = fromPic <$> [notEString1, notEString2, notEString3, notEString4]

notEString1 :: String
notEString1 = "    .-.-.-.-.-.-.-.-.    \n\
              \    |      ^   ^    |    \n\
              \.-. .     .-. .-.   .-.  \n\
              \| |<|     | | | |    v|  \n\
              \.-. .     .-. .-.   .-.  \n\
              \    |               |    \n\
              \.-. .               .    \n\
              \| |>|               |    \n\
              \.-. .-.             . .-.\n\
              \     v|             |<| |\n\
              \    .-.   .-.       . .-.\n\
              \    |     | |       |    \n\
              \    .     .-. .-.   .    \n\
              \    |      v  |>|   |    \n\
              \    .-. .-.-.-. .-.-.    \n\
              \      |<|                \n\
              \      .-.                "

notEString2 :: String
notEString2 = "    .-.-.-.-.-.-.-.-.    \n\
              \    |      v   ^    |    \n\
              \.-. .     .-. .-.   .-.  \n\
              \| |>|     | | | |    v|  \n\
              \.-. .     .-. .-.   .-.  \n\
              \    |               |    \n\
              \.-. .               .    \n\
              \| |>|               |    \n\
              \.-. .-.             . .-.\n\
              \     v|             |<| |\n\
              \    .-.   .-.       . .-.\n\
              \    |     | |       |    \n\
              \    .     .-. .-.   .    \n\
              \    |      v  |>|   |    \n\
              \    .-. .-.-.-. .-.-.    \n\
              \      |<|                \n\
              \      .-.                "

notEString3 :: String
notEString3 = "    .-.-.-.-.-.-.-.-.    \n\
              \    |      ^   ^    |    \n\
              \.-. .     .-. .-.   .-.  \n\
              \| |>|     | | | |    ^|  \n\
              \.-. .     .-. .-.   .-.  \n\
              \    |               |    \n\
              \.-. .               .    \n\
              \| |>|               |    \n\
              \.-. .-.             . .-.\n\
              \     v|             |<| |\n\
              \    .-.   .-.       . .-.\n\
              \    |     | |       |    \n\
              \    .     .-. .-.   .    \n\
              \    |      v  |>|   |    \n\
              \    .-. .-.-.-. .-.-.    \n\
              \      |<|                \n\
              \      .-.                "

notEString4 :: String
notEString4 = "    .-.-.-.-.-.-.-.-.    \n\
              \    |      ^   ^    |    \n\
              \.-. .     .-. .-.   .-.  \n\
              \| |>|     | | | |    v|  \n\
              \.-. .     .-. .-.   .-.  \n\
              \    |               |    \n\
              \.-. .               .    \n\
              \| |>|               |    \n\
              \.-. .-.             . .-.\n\
              \     v|             |<| |\n\
              \    .-.   .-.       . .-.\n\
              \    |     | |       |    \n\
              \    .     .-. .-.   .    \n\
              \    |      v  |<|   |    \n\
              \    .-. .-.-.-. .-.-.    \n\
              \      |<|                \n\
              \      .-.                "

trickyString :: String
trickyString = ".-.-.-.-.\n\
               \|       |\n\
               \. .-.-.-.\n\
               \|>|  v   \n\
               \. .-.-.-.\n\
               \|       |\n\
               \.-.-.-.-."

tricky :: Configuration
tricky = fromPic trickyString

trickyString2 :: String
trickyString2 = ".-.-.-.-.\n\
                \|  v    |\n\
                \. .-.-.-.\n\
                \| |  v   \n\
                \. .-.-.-.\n\
                \|       |\n\
                \.-.-.-.-."

tricky2 :: Configuration
tricky2 = fromPic trickyString2
