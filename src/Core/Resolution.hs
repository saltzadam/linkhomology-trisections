{-|
Module      : Core.Resolution
Description : Operations on Resolutions
Copyright   : (c) Adam Saltz, 2020
License     : GPL-3
Maintainer  : saltz.adam@gmail.com
Stability   : experimental
Portability : POSIX

A Resolution is a finite sequence of 0s and 1s.  They are used to label resolutions of planar diagrams.
-}
{-# LANGUAGE DeriveGeneric #-}
module Core.Resolution
-- TODO: export list!
where
import GHC.Generics (Generic)
import Data.List (unfoldr)
import Core.Util (deleteAtPair)

-- | Another 0-1 data type.
data Resolution' = ZeroR | OneR deriving (Eq,Ord,Generic,Show,Read)
instance Num Resolution' where
    x + ZeroR = x
    ZeroR + x = x
    OneR + OneR = ZeroR

    ZeroR * _ = ZeroR
    _ * ZeroR = ZeroR
    x * OneR  = x

    fromInteger i = if even i then ZeroR else OneR
    negate = id
    abs = id
    signum ZeroR = 0
    signum OneR = 1

type Resolution = [Resolution']

-- | The sum of the 1s in a 'Resolution'.
weight :: Resolution -> Int
weight (ZeroR:xs) = weight xs
weight (OneR:xs) = 1 + weight xs
weight [] = 0

deleteResAt :: Resolution -> (Int, Int) -> Resolution
deleteResAt = flip deleteAtPair

-- | Named out of frustration.
why :: Resolution' -> Int
why ZeroR = 0
why OneR = 1

-- | Converts @Integral a@ to @Resolution'@.
intToR :: Integral a => a -> Resolution'
intToR i = if even i then ZeroR else OneR

hashr :: Resolution -> Int
hashr r = hash' 0 (fmap why r) where
    hash' :: Int -> [Int] -> Int
    hash' i (x:xs) = x * (2 ^ i) + hash' (i+1) xs
    hash' _ [] = 0


-- instance Show Resolution' where
--     show ZeroR = "0"
--     show OneR = "1"

-- | 
rconvert :: Int -> Resolution
rconvert i =  rconvert' $  fmap (read . return) . toBin2 $ i

rconvert' :: [Int] -> [Resolution']
rconvert' (0:xs) = ZeroR:rconvert' xs
rconvert' (1:xs) = OneR :rconvert' xs
rconvert' [] = []
rconvert' _ = error "bad resolution"

toBin2 :: Int -> String
toBin2 = foldMap show . reverse . toBase 2
    where toBase base = unfoldr modDiv where
            modDiv 0 = Nothing
            modDiv n = let (q, r) = (n `divMod` base) in Just (r, q)
  
    

