module Eg.Eg
where

import Core.Tangle
import Core.Braids
import Core.Configuration
import Classification.Tester
import Core.PlanarDiagram
import Algebra.V
import Algebra.Z2

-- usual . and - and | and X and +
-- p for plat
-- ^ and V for up and down on bottom

cap :: [Char]
cap =   ".p.\n\
        \| |\n\
        \^ V"


caps1 :: [Char]
caps1 = ".p. .p.\n\
        \| | | |\n\
        \^ V ^ V"

caps :: [Char]
caps  = ".-. .-.\n\
        \| | | |\n\
        \. . . ."

caps2 :: [Char]
caps2 = ".-.p.-.\n\
        \|     |\n\
        \. .p. .\n\
        \| | | |\n\
        \^ V ^ V"

caps2' :: OrientedMorseTangle
caps2' = halfPlat (Braid 4 [2,1])  [Up,Up,Down,Down]

b1 :: OrientedMorseTangle
b1 = halfPlat (Braid 4 [2,2,1,-3]) [Up, Down, Up, Down]
b1' :: MorseTangle
b1' = omtToMt b1

b1a :: OrientedMorseTangle
b1a = halfPlat (Braid 4 [1,3,-3,2]) [Up, Down, Up, Down]
b1a' :: MorseTangle
b1a' = omtToMt b1a

b3 :: OrientedMorseTangle
b3 = halfPlat (Braid 6 [2,2,1,-3,-4,5,1,1,-3,3,-4]) [Up, Down, Up, Down, Up, Down]
b2' :: MorseTangle
b2' = omtToMt b2

b3' :: MorseTangle
b3' = omtToMt b3
{-
 -   - 
| | | |
|  X  |
|  X  |
 X  | |
| |  X
+ - + -
-}


b2 :: OrientedMorseTangle
b2 = halfPlat (Braid 2 [1]) [Up, Down]

{-
 - 
| |
 x
. .

-}

omt :: OrientedMorseTangle
omt =  stringToOrientedMorseTangle cap
omt1 :: OrientedMorseTangle
omt1 = stringToOrientedMorseTangle caps1
omt2 :: OrientedMorseTangle
omt2 = stringToOrientedMorseTangle caps2
mt1 :: MorseTangle
mt1 = omtToMt omt1
mt2 :: MorseTangle
mt2 = omtToMt omt2


conf :: Configuration
conf = fromPic  ".-.\n\
                \|>|\n\
                \.-."

l :: Labeling
l = makeLabel (const ZZero) (pdComponents . diagram $ conf)

om1 :: OrientedMorseTangle
om1 = halfPlat twotwistspunb1 [Up,Up,Down,Down,Up,Up,Down,Down]
om2 :: OrientedMorseTangle
om2 = halfPlat twotwistspunb2 [Up,Up,Down,Down,Up,Up,Down,Down]
om3 :: OrientedMorseTangle
om3 = halfPlat twotwistspunb3 [Up,Up,Down,Down,Up,Up,Down,Down]
 

twotwistspunb1 :: Braid
twotwistspunb1 = Braid 8 [2,-6] 
twotwistspunb2 :: Braid
twotwistspunb2 = Braid 8 [2,1,-6,-7]
twotwistspunb3 :: Braid
twotwistspunb3 = Braid 8 [2,1,-6,-7,4,4,4,-5,-6,3,2,-4,-4,-4]

{-twospunb1 :: Braid
twospunb1 = Braid 8 [-4,-5,-6,-7-}
