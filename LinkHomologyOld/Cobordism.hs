module LinkHomology.Cobordism where
import           LinkHomology.LinkHomology
import           Core.PlanarDiagram
import           Core.Cube
import           Complex.BuildCube
import           Algebra.V
import           Core.Resolution
import           Core.CubeUtil
import           Core.Configuration

import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Map.Lazy ( Map )
import qualified Data.Map.Lazy                      as M
import           Data.Semigroup

newtype MapOfLinearCubes = MLC {theMap :: LinearCubeOfResolutions Labeling} deriving (Eq, Ord)

applyMapFlatUnsafe :: MapOfLinearCubes -> Labeling -> Maybe (Set Labeling)
applyMapFlatUnsafe mlc l =   M.lookup [OneR] . (M.! l) . (M.! [ZeroR]) . arrows . theMap $ mlc

addOneRes :: Resolution -> Resolution
addOneRes r = r ++ [OneR]

addZeroRes :: Resolution -> Resolution
addZeroRes r = r ++ [ZeroR]

restrictLastResTo
  :: Resolution' -> MapOfLinearCubes -> LinearCubeOfResolutions Labeling
restrictLastResTo res' =
  LinearCubeOf
    . fmap (fmap (M.filterWithKey (\r' _ -> (== res') . last $ r')))
    . M.filterWithKey (\r _ -> (== res') . last $ r)
    . arrows
    . theMap

domain :: MapOfLinearCubes -> LinearCubeOfResolutions Labeling
domain = restrictLastResTo ZeroR
codomain :: MapOfLinearCubes -> LinearCubeOfResolutions Labeling
codomain = restrictLastResTo OneR

mixedmain :: MapOfLinearCubes -> LinearCubeOfResolutions Labeling
mixedmain =
  LinearCubeOf
    . fmap (fmap (M.filterWithKey (\r' _ -> (== OneR) . last $ r')))
    . M.filterWithKey (\r _ -> (== ZeroR) . last $ r)
    . arrows
    . theMap

pushToDomain
  :: Map Resolution (Map Labeling (Map Resolution a))
  -> Map Resolution (Map Labeling (Map Resolution a))
pushToDomain = M.mapKeys addZeroRes . fmap (fmap (M.mapKeys addZeroRes))
pushToCodomain
  :: Map Resolution (Map Labeling (Map Resolution a))
  -> Map Resolution (Map Labeling (Map Resolution a))
pushToCodomain = M.mapKeys addOneRes . fmap (fmap (M.mapKeys addOneRes))
pushToMaplike
  :: Map Resolution (Map Labeling (Map Resolution a))
  -> Map Resolution (Map Labeling (Map Resolution a))
pushToMaplike = M.mapKeys addZeroRes . fmap (fmap (M.mapKeys addOneRes))


-- last dimension is map dimension
makeMap
  :: LinearCubeOfResolutions Labeling
  -> LinearCubeOfResolutions Labeling
  -> ((Resolution, Labeling) -> Set (Resolution, Labeling))
  -> MapOfLinearCubes
makeMap dom codom function
  = let
      f            = curry function
      domainAt0    = pushToDomain . arrows $ dom
      codomainAt1  = pushToCodomain . arrows $ codom
      chainsDomain = chainGroup dom
      map01 =
        pushToMaplike
        . fmap
            ( fmap
                (M.fromListWith S.union . fmap (fmap S.singleton) . S.toList)
            . M.fromList
            . S.toList
            ) -- with?
        . M.mapWithKey (\r ls -> S.map (\l -> (l, f r l)) ls)
        $ chainsDomain :: Map
            Resolution
            (Map Labeling (Map Resolution (Set Labeling)))
    in
      MLC $ LinearCubeOf
        (domainAt0 `addArrowsMod2` codomainAt1 `addArrowsMod2` map01)

-- assumes the maps are made from makeMap
composeMaps :: MapOfLinearCubes -> MapOfLinearCubes -> MapOfLinearCubes
composeMaps map1 map2 =
  (MLC . LinearCubeOf)
    $               fmap (fmap (addMapMod2s . S.toList . S.unions . M.elems))
    .               go map201
    $               map101
    `addArrowsMod2` (arrows . domain $ map1)
    `addArrowsMod2` (arrows . codomain $ map2)
 where
  map101 = arrows . mixedmain $ map1 -- map-like parts
  map201 = arrows . mixedmain $ map2 -- 
  -- we have (r,l) in the codomain of m1.  So r looks like [r',OneR].
  -- we want to look for it in m2, so we'd better adjustR it to [r',ZeroR]
  go m1 m2 = fmap
    (fmap (M.mapWithKey (\r ls -> (S.map (\l -> apply (adjustR r) l m2) ls))))
    m1
  apply
    :: Resolution
    -> Labeling
    -> Map Resolution (Map Labeling (Map Resolution (Set Labeling)))
    -> Map Resolution (Set Labeling)
  apply r l map' = fromJustMap (M.lookup r map' >>= M.lookup l) -- look up all the stuff from (r,l)
                                                                -- note that we already restrictetd to the maplike part
  fromJustMap :: Maybe (Map a b) -> Map a b
  fromJustMap Nothing  = M.empty
  fromJustMap (Just a) = a
  adjustR :: Resolution -> Resolution
  adjustR = reverse . (ZeroR :) . tail . reverse

instance Semigroup MapOfLinearCubes where
  (<>) = composeMaps

-- two-handle attachments don't depend on the particular theory
twoHandleAttachment'
  :: Component -> (Resolution, Labeling) -> Set (Resolution, Labeling)
twoHandleAttachment' comp (r, l) = S.singleton (r, capOff comp l)
 where
  capOff :: Component -> Labeling -> Labeling
  capOff comp' label' = if M.member comp' (getLabeling label')
    then Labeling (M.delete comp . getLabeling $ label')
    else error "capped off a component which isn't there!"

twoHandleAttachment
  :: LinkHomologyTheory -> Component -> CrossPD -> CrossPD -> MapOfLinearCubes
twoHandleAttachment lht comp pd pd' =
  let c1 = chainComplex lht pd
      c2 = chainComplex lht pd'
  in  makeMap c1 c2 (twoHandleAttachment' comp)

oneHandleAttachment'
  :: LinkHomologyTheory
  -> Decoration
  -> CrossPD
  -> (Resolution, Labeling)
  -> Set (Resolution, Labeling)
oneHandleAttachment' lht dec pd (r, l) =
  S.fromList
    . concatMap sequence
    . M.toList
    . fmap (S.toList . ($ l) . applyAction . configurationMap lht . addDeco dec)
    . M.unions
    . M.elems
    . (M.! r)
    . arrows'
    . cubeOfConfigurations
    $ pd
 where
  addDeco :: Decoration -> Configuration -> Configuration
  addDeco dec conf = rebuildConfig $ conf { decos = dec `S.insert` decos conf }

oneHandleAttachment
  :: LinkHomologyTheory -> Decoration -> CrossPD -> MapOfLinearCubes
oneHandleAttachment lht dec pd =
  let c1 = chainComplex lht pd
      c2 = chainComplex lht . decorationActionCrossPD dec $ pd
  in  makeMap c1 c2 (oneHandleAttachment' lht dec pd)

