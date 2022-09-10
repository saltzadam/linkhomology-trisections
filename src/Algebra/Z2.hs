{-|
Module      : Algebra.Z2
Description : The Khovanov chain group
Copyright   : (c) Adam Saltz, 2020
License     : GPL-3
Maintainer  : saltz.adam@gmail.com
Stability   : experimental
Portability : POSIX

Arithmetic over Z/2Z, the field of two elements.
-}
module Algebra.Z2
  (Z2(..),
  z2ToInt)
where

-- | A data type representing Z/2Z, the field with two elements.
data Z2 = ZZero | ZOne deriving (Eq, Ord,Read,Show)

-- | Set @negate@, @signum@, and @abs@ to @id@.
instance Num Z2 where
    (+) ZZero x = x
    (+) x ZZero = x
    (+) ZOne ZOne = ZZero
    (*) ZZero _ = ZZero
    (*) _ ZZero = ZZero
    (*) ZOne x = x
    negate = id
    signum = id
    abs = id
    fromInteger 0 = ZZero
    fromInteger 1 = ZOne
    fromInteger n = fromInteger (n `mod` 2)

-- | @z2ToInt ZZero == 0@ and @z2ToInt ZOne == 1@.
z2ToInt :: Z2 -> Int
z2ToInt ZZero = 0
z2ToInt ZOne = 1

-- -- performance?
-- mod2addS :: Ord a => a -> Set a -> Set a
-- mod2addS x s = if x `S.member` s 
--                then S.delete x s
--                else S.insert x s

-- mod2addL :: Eq a => a -> [a] -> [a]
-- mod2addL x s = if x `elem` s
--                then delete x s 
--                else x:s 

-- mod2NEL :: Eq a => a -> NonEmpty a -> Maybe (NonEmpty a)
-- mod2NEL x s = if x `elem` s
--               then nelDelete x s
--               else Just s

-- nelDelete :: Eq b => b -> NonEmpty b -> Maybe (NonEmpty b)
-- nelDelete x = NEL.nonEmpty . delete x . NEL.toList 

