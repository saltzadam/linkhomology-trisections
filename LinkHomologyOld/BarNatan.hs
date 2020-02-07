module LinkHomology.BarNatan
  ( barNatanOneHandleMapSimple
  , barNatanOneHandleMap
  , bn
  , bnSimple
  , bnFull
  )
where
import           Core.Configuration
import           Core.Configuration.Orientation
import           LinkHomology.LinkHomology
import           Core.Util
import           Core.PlanarDiagram
import           Algebra.V
import           Algebra.Z2
import           Classification.Tree
import           Core.Tangle
import           Core.Grid

import qualified Data.Set                      as S
import qualified Data.Map.Lazy as M
import           Data.Maybe

----

bnSimple :: LinkHomologyTheory
bnSimple = LHT barNatanOneHandleMapSimple

bn :: LinkHomologyTheory
bn = bnSimple

bnFull :: LinkHomologyTheory
bnFull = LHT barNatanOneHandleMap

barNatanOneHandleMapSimple :: Configuration -> DecorationMap
barNatanOneHandleMapSimple config
  | (S.size . decos $ config) /= 1 = DecorationMap M.empty
  | (S.size . activeCircles $ config) == 1 = split
    (head . S.toList . activeCircles $ config)
    (head . S.toList . decos $ config)
  | otherwise = merge (listToTuple . S.toList . activeCircles $ config)
                      (head . S.toList . decos $ config)

split :: Component -> Decoration -> DecorationMap
split comp deco =
  let sourceConfig = buildConfig (S.singleton comp) (S.singleton deco)
      targetConfig = orientConfigCCW <$> decorationAction deco sourceConfig
      plusSource   = makeLabel (const2 ZOne) sourceConfig
      plusTarget   = makeLabel (const2 ZOne) <$> targetConfig
  in  DecorationMap
      . M.fromList
      $ [(plusSource, S.fromList . catMaybes $ [plusTarget])]

merge :: (Component, Component) -> Decoration -> DecorationMap
merge (c1, c2) deco =
  let sourceConfig = buildConfig (S.fromList [c1, c2]) (S.singleton deco)
      targetConfig = orientConfigCCW' <$> decorationAction deco sourceConfig
  in  DecorationMap $ M.fromList
        [ ( makeLabel (const2 ZZero) sourceConfig
          , S.fromList . catMaybes $ [makeLabel (const2 ZZero) <$> targetConfig]
          )
        ]
----

barNatanOneHandleMap :: Configuration -> DecorationMap
barNatanOneHandleMap config =
  let confs = parseUnionOfTreesAndDualTrees config
  in  maybe (DecorationMap M.empty) (hconcat . fmap barNatanOneHandleMap') confs
-- h for horizontal
-- <> for Labelings is horizontal!
-- TODO: replace with non-empty!

hconcat :: [DecorationMap] -> DecorationMap
hconcat [d     ] = d
hconcat (d : ds) = d `happend` hconcat ds

happend :: DecorationMap -> DecorationMap -> DecorationMap
happend d d' =
  let xs = M.toList . action $ d
      ys = M.toList . action $ d'
  in  DecorationMap $ M.fromList
        [ (x `mappend` y, S.map (uncurry mappend) . S.cartesianProduct x' $ y')
        | (x, x') <- xs
        , (y, y') <- ys
        ]

barNatanOneHandleMap' :: (Configuration, TreeType) -> DecorationMap
barNatanOneHandleMap' (config, Tree    ) = treeMap config
barNatanOneHandleMap' (config, DualTree) = dualTreeMap config

-- not sure if this is the right way to do this.  point is that we've already validated that the map is a Tree or DualTree by splitting into configurations, so it seems silly to buildConfig after that.
-- TODO: figure out if the other maps should be refactored like this.  probably doesn't matter
-- this is exactly the same definition as split!  which is great!!

-- | assumes map is already validated as a tree!
treeMap :: Configuration -> DecorationMap
treeMap conf =
  let sourceConfig = conf
      targetConfig = fmap orientConfigCCW' . applyAllDecorations $ conf
  in  DecorationMap $ M.fromList
        [ ( makeLabel (const2 ZZero) sourceConfig
          , S.fromList . catMaybes $ [makeLabel (const2 ZZero) <$> targetConfig]
          )
        ]

dualTreeMap :: Configuration -> DecorationMap
dualTreeMap conf =
  let sourceConfig = conf
      targetConfig = fmap orientConfigCCW' . applyAllDecorations $ conf
  in  DecorationMap $ M.fromList
        [ ( makeLabel (const2 ZOne) sourceConfig
          , S.fromList . catMaybes $ [makeLabel (const2 ZOne) <$> targetConfig]
          )
        ]


