module LinkHomology.BarNatan
  -- ( bnOneHandleMap,
  --   bnSimple,
  --   khOneHandleMap,
  --   kh
  -- )
                             where
import           Core.Configuration
-- import           Core.Configuration.Orientation
import           LinkHomology.LinkHomology
import           Core.Util
import           Core.PlanarDiagram
import           Algebra.V
import           Algebra.Z2
import           Classification.Tree
import           Algebra.Graph.AdjacencyMap

import qualified Data.Set                      as S
import           Data.Set                       ( Set )
import           Data.Maybe
import qualified Data.Map                      as M

----

bnSimple :: LinkHomologyTheory
bnSimple = LHT barNatanOneHandleMapSimple

bn :: LinkHomologyTheory
bn = bnSimple

bnFull :: LinkHomologyTheory
bnFull = LHT bnOneHandleMap

barNatanOneHandleMapSimple :: ConfigurationMap
barNatanOneHandleMapSimple config
  | (S.size . decos $ config) == 0 = S.singleton
  | (S.size . decos $ config) == 1 = if
    | (S.size . vertexSet . activePart $ config) == 1 -> splitMap config
    | otherwise -> mergeMap config
  | otherwise = const S.empty


bnOneHandleMap :: ConfigurationMap
bnOneHandleMap config
  | (S.size . decos $ config) == 0 = S.singleton
  | -- identity, not zero
    (S.size . decos $ config) == 1 = if
    | (S.size . vertexSet . activePart $ config) == 1 -> splitMap config
    | otherwise -> mergeMap config
  | otherwise = higherBarNatanOneHandleMap config


splitMap :: Configuration -> LinearMap
splitMap conf =
  split (head . vertexList . activePart $ conf) (head . S.toList . decos $ conf)

mergeMap :: Configuration -> LinearMap
mergeMap conf = if S.size (vertexSet $ activePart conf) /= 2
  then
    error
    $  "only had "
    ++ (show . S.size . vertexSet $ activePart conf)
    ++ " circles for merge map: "
    ++ show conf
  else merge (listToTuple . vertexList . activePart $ conf)
             (head . S.toList . decos $ conf)

kh :: LinkHomologyTheory
kh = LHT khOneHandleMap

khOneHandleMap :: ConfigurationMap
khOneHandleMap config
  | (S.size . decos $ config) == 1 = if
    | (S.size . vertexSet . activePart $ config) == 1 -> khSplitMap config
    | otherwise -> khMergeMap config
  | otherwise = const S.empty


khSplitMap :: Configuration -> LinearMap
khSplitMap conf = khSplit (head . vertexList . activePart $ conf)
                          (head . S.toList . decos $ conf)

khSplit :: PD -> Decoration -> LinearMap
khSplit comp deco l =
  let sourceConfig = buildConfig (S.singleton comp) (S.singleton deco)
      targetConfig = fromJust $ decorationAction deco sourceConfig
      sourceComps  = S.toList . components $ sourceConfig
      targetComps  = S.toList . components $ targetConfig
      plusSource   = makeLabel (const ZOne) sourceComps
      plusTargets =
        makeLabel (const ZOne) targetComps `S.delete` allLabels targetConfig
      minusSource = makeLabel (const ZZero) sourceComps
      minusTarget = makeLabel (const ZZero) targetComps
  in  if
        | l == plusSource  -> plusTargets
        | l == minusSource -> S.singleton minusTarget
        | otherwise        -> S.empty


khMergeMap :: Configuration -> LinearMap
khMergeMap conf = khMerge (listToTuple . vertexList . activePart $ conf)
                          (head . S.toList . decos $ conf)


khMerge :: (PD, PD) -> Decoration -> LinearMap
khMerge (c1, c2) deco l =
  let sourceConfig = buildConfig (S.fromList [c1, c2]) (S.singleton deco)
      targetConfig = fromJust $ decorationAction deco sourceConfig
      sourceComps  = pdComponents . diagram $ sourceConfig
      targetComps  = pdComponents . diagram $ targetConfig
  in  if
        | l == makeLabel (const ZZero) sourceComps -> S.empty
        | l == makeLabel (const ZOne) sourceComps -> S.fromList
          [ makeLabel (const ZOne)  targetComps
          , makeLabel (const ZZero) targetComps
          ]
        | otherwise -> S.singleton (makeLabel (const ZZero) targetComps)




-- | these "include" Kh as well
split :: PD -> Decoration -> LinearMap
split comp deco l =
  let sourceConfig = buildConfig (S.singleton comp) (S.singleton deco)
      targetConfig =
        fromMaybe
            (  error
            $  "something went wrong applying decorationAction in split: "
            ++ show sourceConfig
            ++ "\n"
            ++ show deco
            )
            (decorationAction deco sourceConfig)
      sourceComps = pdComponents . diagram $ sourceConfig
      targetComps = pdComponents . diagram $ targetConfig
      plusSource  = makeLabel (const ZOne) sourceComps
      plusTarget  = makeLabel (const ZOne) targetComps
      minusSource = makeLabel (const ZZero) sourceComps
      minusTarget = makeLabel (const ZZero) targetComps
  in  if
        | l == plusSource  -> S.singleton plusTarget
        | l == minusSource -> S.singleton minusTarget
        | otherwise        -> S.empty

-- | these "include" Kh as well
merge :: (PD, PD) -> Decoration -> LinearMap
merge (c1, c2) deco l =
  let sourceConfig = buildConfig (S.fromList [c1, c2]) (S.singleton deco)
      sourceComps  = pdComponents . diagram $ buildConfig
        (S.fromList [c1, c2])
        (S.singleton deco)
      targetComps =
        pdComponents
          . diagram
          . fromMaybe
              (  error
              $  "something went wrong applying decorationAction in merge: "
              ++ show sourceConfig
              ++ "\n"
              ++ show deco
              )
          $ decorationAction deco sourceConfig
  in  if
        | l == makeLabel (const ZZero) sourceComps -> S.singleton
          (makeLabel (const ZZero) targetComps)
        | l == makeLabel (const ZOne) sourceComps -> S.singleton
          (makeLabel (const ZOne) targetComps)
        | otherwise -> S.empty
        --  DecorationMap $ M.fromList
        -- [ ( makeLabel (const ZZero) sourceConfig
        --   , S.fromList . catMaybes $ [makeLabel (const ZZero) <$> targetConfig]
        --   )
        -- , ( makeLabel (const ZOne) sourceConfig
        --   , S.fromList . catMaybes $ [makeLabel (const ZOne) <$> targetConfig]
        --   )
        -- ]
----

higherBarNatanOneHandleMap :: ConfigurationMap
higherBarNatanOneHandleMap config l =
  let subconfs =
        parseUnionOfTreesAndDualTrees config :: Maybe
            [(Configuration, TreeType)]
      bigDomain =
        maybe S.empty (S.fromList . combineLabelings . fmap bnDomain) subconfs
      bigTarget =
        maybe S.empty (S.fromList . combineLabelings . fmap bnTarget) subconfs
  in  if l `S.member` bigDomain then bigTarget else S.empty


combineLabelings :: [Set Labeling] -> [Labeling]
combineLabelings =
  fmap (Labeling . M.unions . fmap getLabeling) . S.toList . cartesianProducts
bnDomain :: (Configuration, TreeType) -> Set Labeling
bnDomain (config, Tree) = allLabels config
bnDomain (config, DualTree) =
  S.singleton $ makeLabel (const ZOne) (pdComponents . diagram $ config)

bnTarget :: (Configuration, TreeType) -> Set Labeling
bnTarget (config, Tree) =
  S.singleton
    . makeLabel (const ZZero)
    . (pdComponents . diagram)
    . fromJust
    $ applyAllDecorations config
bnTarget (config, DualTree) =
  allLabels . fromJust . applyAllDecorations $ config

barNatanOneHandleMap' :: (Configuration, TreeType) -> LinearMap
barNatanOneHandleMap' (config, Tree    ) = treeMap config
barNatanOneHandleMap' (config, DualTree) = dualTreeMap config

-- not sure if this is the right way to do this.  point is that we've already validated that the map is a Tree or DualTree by splitting into configurations, so it seems silly to buildConfig after that.
-- TODO: figure out if the other maps should be refactored like this.  probably doesn't matter
-- this is exactly the same definition as split!  which is great!!

-- | assumes map is already validated as a tree!
treeMap :: ConfigurationMap
treeMap conf _ =
  let targetConfig = fromJust . applyAllDecorations $ conf
  in  S.singleton
        (makeLabel (const ZZero) (pdComponents . diagram $ targetConfig))

-- | assumes map is already validated as a dual tree!
dualTreeMap :: ConfigurationMap
dualTreeMap conf l =
  let targetConf = fromJust . applyAllDecorations $ conf
  in  if l == makeLabel (const ZOne) (pdComponents . diagram $ conf)
        then allLabels targetConf
        else S.empty


