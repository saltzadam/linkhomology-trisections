{-|
Module      : Classification.Tree
Description : Test for tree and dual tree configurations
Copyright   : (c) Adam Saltz, 2020
License     : GPL-3
Maintainer  : saltz.adam@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Classification.Tree where
import           Core.Configuration
-- import Core.Configuration.Orientation
import           Core.Util

import qualified Data.Set                      as S

data TreeType = Tree | DualTree deriving (Show, Eq, Ord)

-- | Is a 'Configuration' a tree?
isTree :: Configuration -> Bool
isTree config =
  maybe False ((== 1) . S.size . components) (applyAllDecorations config)

-- | Is a 'Configuration' a dual tree?
-- note this only works with parseUnionOfTreesAndDualTrees
isDualTree :: Configuration -> Bool
isDualTree config = S.size (components config) == 1 && maybe
  False
  (((==) . (+ 1) . S.size . decos $ config) . S.size . components)
  (applyAllDecorations config)

-- | Separate a configuration into maximal active subconfigurations, then check if each is a tree or dual tree.
parseUnionOfTreesAndDualTrees
  :: Configuration -> Maybe [(Configuration, TreeType)]
parseUnionOfTreesAndDualTrees conf =
  traverse sequence . fmap parse $ connectedActiveConfigs conf
 where
  parse' :: Configuration -> Maybe TreeType
  parse' c | isTree c     = Just Tree
           | isDualTree c = Just DualTree
           | otherwise    = Nothing
  parse = graph parse'
