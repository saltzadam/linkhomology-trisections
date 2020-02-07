module LinkHomology.Cobordism where
import           LinkHomology.LinkHomology
import           Core.PlanarDiagram
import           Core.Cube
import           Algebra.V
import           Core.Resolution

import           Data.Semigroup
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as M

newtype MapOfCubes index object morphism = MC {theMap :: CubeOf index object morphism }

type OurMap m = MapOfCubes Resolution Labeling m

type FOnLabelings m = m -> Labeling -> Set Labeling

-- this issue here is that I'm stuck between saying o = Labeling and o = PD
-- right way to do things, for now, is to set o = Labeling and (Ring m)

-- 'composition' of two configurations is not a configuration!

addOneRes :: Resolution -> Resolution
addOneRes r = r ++ [OneR]

addZeroRes :: Resolution -> Resolution
addZeroRes r = r ++ [ZeroR]

restrictLastResTo :: Resolution' -> OurMap m -> CubeOf Resolution Labeling m
restrictLastResTo res' =
  CubeOf
    . fmap (fmap (M.filterWithKey (\r' _ -> (== res') . last $ r')))
    . M.filterWithKey (\r _ -> (== res') . last $ r)
    . arrows'
    . theMap

domain :: OurMap m -> CubeOf Resolution Labeling m
domain = restrictLastResTo ZeroR
codomain :: OurMap m -> CubeOf Resolution Labeling m
codomain = restrictLastResTo OneR

mixedmain :: OurMap m -> CubeOf Resolution Labeling m
mixedmain =
  CubeOf
    . fmap (fmap (M.filterWithKey (\r' _ -> (== OneR) . last $ r')))
    . M.filterWithKey (\r _ -> (== ZeroR) . last $ r)
    . arrows'
    . theMap

pushToDomain
  :: Map Resolution (Map o (Map Resolution m))
  -> Map Resolution (Map o (Map Resolution m))
pushToDomain = M.mapKeys addZeroRes . fmap (fmap (M.mapKeys addZeroRes))
pushToCodomain
  :: Map Resolution (Map o (Map Resolution m))
  -> Map Resolution (Map o (Map Resolution m))
pushToCodomain = M.mapKeys addOneRes . fmap (fmap (M.mapKeys addOneRes))
pushToMaplike
  :: Map Resolution (Map o (Map Resolution m))
  -> Map Resolution (Map o (Map Resolution m))
pushToMaplike = M.mapKeys addZeroRes . fmap (fmap (M.mapKeys addOneRes))


-- last dimension is map dimension
makeMap
  :: (Ring m)
  => CubeOf Resolution Labeling m
  -> CubeOf Resolution Labeling m
  -> ((Resolution, Labeling) -> Map Resolution m)
  -> OurMap m
makeMap dom codom f =
  let domainAt0    = pushToDomain . arrows' $ dom
      codomainAt1  = pushToCodomain . arrows' $ codom
      domainChains = fmap M.keys domainAt0
      map01 =
        fmap (M.fromListWith (M.unionWith (<+>)))
          . M.mapWithKey (\r ls -> [ (l, f (r, l)) | l <- ls ])
          $ domainChains
  in  MC . CubeOf $ addAbstractArrows [domainAt0, codomainAt1, map01]
 -- TODO: make cubeutil better so that this works
addAbstractArrows
  :: (Ord a, Ord b, Ord c, Ring d)
  => [Map a (Map b (Map c d))]
  -> Map a (Map b (Map c d))
addAbstractArrows = foldr addAbstractArrows' M.empty
addAbstractArrows'
  :: (Ord a, Ord b, Ord c, Ring d)
  => Map a (Map b (Map c d))
  -> Map a (Map b (Map c d))
  -> Map a (Map b (Map c d))
addAbstractArrows' = M.unionWith (M.unionWith (M.unionWith (<+>)))


composeMaps :: (Ring m) => FOnLabelings m -> OurMap m -> OurMap m -> OurMap m
composeMaps fOnLabels map1 map2 = MC . CubeOf $ addAbstractArrows
  [dom, codom, newArrows]
 where
  dom   = arrows' . domain $ map1
  codom = arrows' . codomain $ map2
  newArrows =
    go fOnLabels (arrows' . mixedmain $ map1) (arrows' . mixedmain $ map2)
  -- we have (r,l) in the codomain of m1.  So r looks like [r',OneR].
  -- we want to look for it in m2, so we'd better adjustR it to [r',ZeroR]
  -- TODO: makes more sense to just forget the last coordinate!

  -- TODO: simplify below, really not clear that it does what it should
  -- TODO: also make sure you want <+> and not <>, etc.
  -- for now, should think o = Labeling
  go
    :: (Ring m)
    => FOnLabelings m -- this gets the target diagram
    -> Map Resolution (Map Labeling (Map Resolution m))
    -> Map Resolution (Map Labeling (Map Resolution m))
    -> Map Resolution (Map Labeling (Map Resolution m))
  go getTarget m1 m2 = fmap
    (fmap (M.unionsWith (<+>)) . M.mapWithKey
      (\o' ->
        (concat . M.elems . M.mapWithKey
          (\r m' ->
            fmap (fmap (m' <<>>) . applyMap (addZeroRes r) m2)
              . S.toList
              . getTarget m'
              $ o'
          )
        )
      )
    )
    m1

  applyMap
    :: Ord o
    => Resolution
    -> Map Resolution (Map o (Map Resolution m))
    -> o
    -> Map Resolution m
  applyMap r map' l = fromJustMap (M.lookup r map' >>= M.lookup l) -- look up all the stuff from (r,l)
  fromJustMap :: Maybe (Map a b) -> Map a b
  fromJustMap Nothing  = M.empty
  fromJustMap (Just a) = a


instance Semigroup (MapOfCubes Resolution Labeling LinearMap) where
   (<>) = composeMaps id

-- TODO: would be nice
-- instance Semigroup (MapOfCubes Resolution PD LinearMap) where
--    (<>) = composeMaps id


-- two-handle attachments don't depend on the particular theory
twoHandleAttachment' :: Component -> Resolution -> Map Resolution LinearMap
twoHandleAttachment' comp r = M.singleton r (S.singleton . capOff comp)
 where
  capOff :: Component -> Labeling -> Labeling
  capOff comp' label' = if M.member comp' (getLabeling label')
    then Labeling (M.delete comp . getLabeling $ label')
    else error "capped off a component which isn't there!"

twoHandleAttachment
  :: LinkHomologyTheory
  -> Component
  -> CrossPD
  -> CrossPD
  -> MapOfCubes Resolution Labeling LinearMap
twoHandleAttachment lht comp pd pd' =
  let c1 = chainComplex' lht pd
      c2 = chainComplex' lht pd'
  in  makeMap c1 c2 (twoHandleAttachment' comp . fst)

-- TODO: make both of these work

-- oneHandleAttachment'
--   :: LinkHomologyTheory
--   -> Decoration
--   -> CrossPD
--   -> Resolution
--   -> Map Resolution LinearMap
-- oneHandleAttachment' lht dec cpd r =
--   fmap (configurationMap lht . addDeco dec)
--     . M.unions
--     . M.elems
--     . (M.! r)
--     . arrows'
--     . cubeOfConfigurations
--     $ cpd
--  where
--   addDeco :: Decoration -> Configuration -> Configuration
--   addDeco dec' conf =
--     rebuildConfig $ conf { decos = dec' `S.insert` decos conf }

-- oneHandleAttachment
--   :: LinkHomologyTheory
--   -> Decoration
--   -> CrossPD
--   -> MapOfCubes Resolution Labeling LinearMap
-- oneHandleAttachment lht dec pd =
--   let c1 = chainComplex' lht pd
--       c2 = chainComplex' lht . decorationActionCrossPD dec $ pd
--   in  makeMap c1 c2 (oneHandleAttachment' lht dec pd . fst)

