module Classification.Tree where
import           Core.Configuration
-- import Core.Configuration.Orientation
import           Core.Util

import qualified Data.Set                      as S

data TreeType = Tree | DualTree deriving (Show, Eq, Ord)

isTree :: Configuration -> Bool
isTree config =
  maybe False ((== 1) . S.size . components) (applyAllDecorations config)

-- note this only works with parseUnionOfTreesAndDualTrees
isDualTree :: Configuration -> Bool
isDualTree config = S.size (components config) == 1 && maybe
  False
  (((==) . (+ 1) . S.size . decos $ config) . S.size . components)
  (applyAllDecorations config)

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
