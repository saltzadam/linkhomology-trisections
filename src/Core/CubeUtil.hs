{-|
Module      : Core.CubeUtil 
Description : Utility functions for working with cubes
Copyright   : (c) Adam Saltz, 2020
License     : GPL-3
Maintainer  : saltz.adam@gmail.com
Stability   : experimental
Portability : POSIX

Implements most of the Z/2Z arithmetic for cubes, etc.
-}
module Core.CubeUtil
(addToSetMod2,
 addSetsMod2,
 addArrowsMod2,
 addArrowsMod2s,
 addMapMod2,
 addMapMod2s,
 sFromListMod2,
 unionsMod2)
where

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.List.Extra (group, sort)

sFromListMod2 :: Ord o => [o] -> Set o
sFromListMod2 = S.fromList . fmap head . filter (odd . length) . group . sort

addToSetMod2 :: (Ord o) => o -> Set o -> Set o
addToSetMod2 x xs = if x `S.member` xs then S.delete x xs else S.insert x xs

-- think this is the closest thing to 'fusion'
addSetsMod2 :: (Ord o) => Set o -> Set o -> Set o
addSetsMod2 xs ys = (xs `S.union` ys) `S.difference` (xs `S.intersection` ys)

unionsMod2 :: (Ord o) => [Set o] -> Set o
unionsMod2 = foldr addSetsMod2 S.empty 

addMapMod2 :: Ord i => Ord o => Map i (Set o) -> Map i (Set o) -> Map i (Set o)
addMapMod2 = M.unionWith S.union

addMapMod2s :: Ord i => Ord o => [Map i (Set o)] -> Map i (Set o)
addMapMod2s = foldr addMapMod2 M.empty

addArrowsMod2 :: (Ord o, Ord i) => Map i (Map o (Map i (Set o))) -> Map i (Map o (Map i (Set o))) -> Map i (Map o (Map i (Set o)))
addArrowsMod2 = M.unionWith (M.unionWith (M.unionWith addSetsMod2))

addArrowsMod2s :: (Ord o, Ord i) => [Map i (Map o (Map i (Set o)))] -> Map i (Map o (Map i (Set o))) 
addArrowsMod2s = foldr addArrowsMod2 M.empty


