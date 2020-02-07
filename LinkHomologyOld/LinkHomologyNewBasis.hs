{-# LANGUAGE MultiParamTypeClasses #-}
module LinkHomology.LinkHomology
  ( applyAction
  , DecorationMap(..)
  , replaceLabels
  , qGrading
  , qGradingSets
  , hGrading
  , LinkHomologyTheory(..)
  , chainComplex
  , rankC
  -- , orientedLabelings
  , orientedLabeling
  )
where
import           Data.Map.Lazy ( Map )
import           Data.Set                       ( Set )
import qualified Data.Map.Lazy               as M
import qualified Data.Set                      as S
import           Data.List                      ( isSubsequenceOf )
import           Control.Arrow                  ( (***) )
import           Data.Maybe                     ( catMaybes )

import           Algebra.V
import           Core.CubeUtil
import           Core.Util
import           Core.Resolution
import           Algebra.Z2
import           Core.Configuration
import           Core.Configuration.Orientation
import           Core.PlanarDiagram
import           Core.Cube
import           Complex.BuildCube
import           Complex.Homology
import           Core.Tangle
import           Core.Grid
import           Complex.Hyperbox

-- ZOne = A
-- ZZero = B

newtype DecorationMap = DecorationMap { action :: Map Labeling (Set Labeling)} deriving (Show, Ord, Eq)

instance Monoid DecorationMap where
  mempty = DecorationMap M.empty
  mappend m m' = DecorationMap (M.unionWith addSetsMod2 (action m) (action m'))

instance Ring DecorationMap where
  rzero = mempty
  (<+>) = mappend
  d <<>> d' = let
    f = action d
    g = action d'
    in
    DecorationMap $ f <<>> g
    where
      -- temp  = fmap (catMaybesSet . S.map (f `M.lookup`)) g


--right place to add coefficients
newtype LinkHomologyTheory = LHT {
                              configurationMap ::  Configuration -> DecorationMap
                              }

rankC :: LinkHomologyTheory -> CrossPD -> Int
rankC c = rank . homology . chainComplex c

chainComplex
  :: LinkHomologyTheory -> CrossPD -> LinearCubeOfResolutions Labeling
chainComplex lht pd =
  let
    go' :: Ord o => Map [o] j -> Map o j
    go' = M.fromList . fmap swap . concatMap sequence . fmap swap . M.toList
  in
    LinearCubeOf
    . fmap
        ( M.mapWithKey
            (\l m -> fmap (\c -> applyAction (configurationMap lht c) l) m)

        . go'
        . M.mapKeys (buildKhovanovGroup . drdToConfig)
        )
    . arrows'
    . cubeOfConfigurations
    $ pd


-- need to be careful with Set and redundancy
applyAction :: DecorationMap -> Labeling -> Set Labeling
applyAction cobord = replaceLabels (action cobord)

replaceLabels :: Map Labeling (Set Labeling) -> Labeling -> Set Labeling
replaceLabels action' label =
  listToSetMod2
    . fmap Labeling
    . catMaybes
    . fmap
        (($ getLabeling label) . replaceSubMap . (getLabeling *** getLabeling))
    . concatMap sequence   -- [(Labeling, Labeling)]
    . M.toList
    . fmap S.toList
    $ action'

replaceSubMap
  :: (Ord k, Eq a) => (Map k a, Map k a) -> Map k a -> Maybe (Map k a)
replaceSubMap (a, b) m = if a `M.isSubmapOf` m
  then Just $ b `M.union` compose (fmap M.delete (M.keys a)) m
-- was M.union b . compose (fmap M.delete (M.keys a)) $ m
  else Nothing

replaceSubMapKeys :: Ord k => Map k a -> Map k a -> Map k a
replaceSubMapKeys a m =
  if M.keys a `isSubsequenceOf` M.keys m then a `M.union` m else m

-- probably slow
-- troubleshooting
-- isSubmapOf :: (Eq k, Hashable k, Eq a, Ord k) => Map k a -> Map k a -> Bool
-- isSubmapOf small large = (M.size small) <= (M.size large) && Map.fromList (M.toList small) `Map.isSubmapOf` Map.fromList (M.toList large)
hGrading :: (Resolution, o) -> Int
hGrading (r, _) = weight r

qGrading :: (Resolution, Labeling) -> Int
qGrading (r, x) = qGrading' x + hGrading (r, x)
 where
  qGrading' :: Labeling -> Int
  qGrading' = sum . fmap (decToInt . snd) . M.toList . getLabeling
  decToInt :: Z2 -> Int
  decToInt ZOne  = 1
  decToInt ZZero = -1

qGradingSets :: (Resolution, Set Labeling) -> Int
qGradingSets (r, ls) = minimum . S.map (curry qGrading r) $ ls


-- assumed here that orientations of bottoms are consistent
orientedLabelings' :: OrientedMorseTangle -> OrientedMorseTangle -> [Component]
orientedLabelings' omt1@(OMTangle t1 b1 p1 o1) omt2 =
  let omt1' = orientedResolution omt1
      omt2' = orientedResolution omt2
      diag  = unsafeCrossToFlat $ glueTangle (tangle omt1') (tangle omt2') -- tangle
                                                                              -- means
                                                                              -- [CrossPD],
                                                                              -- as
                                                                              -- it
                                                                              -- turns
                                                                              -- out
  in  fmap orientComponent' . componentsAsNodes $ diag

orientedLabelings :: OrientedMorseTangle -> OrientedMorseTangle -> Set Labeling
orientedLabelings omt1@(OMTangle _ _ _ o1) omt2 =
  let comps  = orientedLabelings' omt1 omt2
      oNodes = orientationToNodes o1
      compAsPairs (Comp c) = fmap (\(Join p q) -> (p, q)) c
      orientCoherently :: Component -> Node Point -> Component
      orientCoherently comp@(Comp c) n@(Join p q) = if n `elem` c
        then if (p, q) `elem` compAsPairs comp
          then comp
          else reverseOrientation comp
        else comp
      -- TODO: make this not quadratic
      orientedRes = compose (fmap (fmap . flip orientCoherently) oNodes) comps
      config      = buildConfig (S.fromList orientedRes) S.empty
      orientationNumber comp = if getOrientation comp == CCW then 0 else 1
      rasmussenLevel :: Configuration -> Component -> Int
      rasmussenLevel conf comp =
        nestingLevel (getPD conf) comp + orientationNumber comp
  in  S.fromList
      . fmap Labeling
      . traverse (\r -> if odd r then [ZOne] else [ZZero])
      . graphMap (rasmussenLevel config)
      $ comps


-- assumes oriented
orientedLabeling :: PD -> Set Labeling
orientedLabeling pd =
  let comps = componentsAsNodes pd
      rasmussenLevel comp = nestingLevel pd comp + orientationNumber comp
      orientationNumber comp = if getOrientation comp == CCW then 0 else 1
  in  S.fromList . fmap Labeling
      . traverse ((\r -> if odd r then [ZOne] else [ZZero]))
      . graphMap rasmussenLevel
      $ comps
