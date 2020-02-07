{-# LANGUAGE ScopedTypeVariables #-}
module Complex.Homology
-- (homology)
                        where
import           Core.Cube
import           Core.CubeUtil
import           Core.Resolution

import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Maybe                     ( listToMaybe )

-- what should this have?
newtype HomologyOptions = HO {i :: Int}

homologyPage
  :: (Show d, Ord d)
  => LinearCubeOfResolutions d
  -> Int
  -> LinearCubeOfResolutions d
homologyPage cube page =
  -- let pruned = CubeOf (pruneMap . arrows $ cube) in
  if M.null (arrows cube) then cube else homology'' 1 page cube

homology
  :: (Ord o, Show o) => LinearCubeOfResolutions o -> LinearCubeOfResolutions o
homology cube =
  let maxStep = maxLen cube
                                                          -- pruned  = CubeOf  (pruneMap . arrows $ cube)
  in  if M.null (arrows cube) then cube else homology'' 1 maxStep cube -- TODO: do we need this condition?

homology''
  :: (Ord o, Show o)
  => Int
  -> Int
  -> LinearCubeOfResolutions o
  -> LinearCubeOfResolutions o
homology'' step maxStep cube = if step > maxStep
  then cube
  else case getAnArrowHGrading cube step of
    Just arr ->
      let cube' = cancelArrow arr cube in homology'' step maxStep cube'
    Nothing -> homology'' (step + 1) maxStep cube

maxLen :: LinearCubeOfResolutions o -> Int
maxLen = (+ 2) . length . head . M.keys . chainGroup -- TODO: should this have (2^)?

pruneCube :: LinearCubeOf i o -> LinearCubeOf i o
pruneCube = amap (fmap (fmap (M.filter (not . S.null))))

-- cube is now
-- Map resolution (Map object (Map resolution (Set object)))
type Arrow o = (Resolution, o, Resolution, o)
source :: Arrow o -> (Resolution, o)
source (r, o, _, _) = (r, o)
target :: Arrow o -> (Resolution, o)
target (_, _, r', o') = (r', o')
targetR :: Arrow o -> Resolution
targetR = fst . target
targetG :: Arrow o -> o
targetG = snd . target
sourceR :: Arrow o -> Resolution
sourceR = fst . source
sourceG :: Arrow o -> o
sourceG = snd . target

arrowRank :: LinearCubeOfResolutions o -> Int
arrowRank =
  sum
    . concatMap M.elems
    . fmap (fmap S.size)
    . concatMap M.elems
    . M.elems
    . arrows

getAnArrowHGrading :: LinearCubeOfResolutions o -> Int -> Maybe (Arrow o)
getAnArrowHGrading c len =
  getAnArrowGrading (\r _ r' _ -> weight r' - weight r <= len) c

getAnArrowNoGrading :: LinearCubeOfResolutions o -> Maybe (Arrow o)
getAnArrowNoGrading = getAnArrowGrading (\_ _ _ _ -> True)

getAnArrowGrading
  :: (Resolution -> o -> Resolution -> o -> Bool)
  -> LinearCubeOfResolutions o
  -> Maybe (Arrow o)
getAnArrowGrading g c = do
  let
    candidates =
      M.filter (not . M.null) -- removes resolutions without generators
        . fmap (M.filter (not . M.null) -- removes generators without targets
                                        . fmap (M.filter (not . S.null))) -- removes target resolutions without targets
        . M.mapWithKey -- grading
            (\r m ->
              (M.mapWithKey
                (\o m' -> M.mapWithKey (\r' os -> S.filter (g r o r') os) m')
                m
              )
            )
        . fmap (M.filter (not . M.null))
        . arrows
        $ c
  m1  <- listToMaybe . M.toList $ candidates
  m2' <- traverse (listToMaybe . M.toList) m1
  let m2 = (\(r, (o, m)) -> ((r, o), m)) m2'
  m3 <- traverse (listToMaybe . M.toList) m2
  fmap (\((a, b), (c', d)) -> (a, b, c', d))
    . traverse sequence
    . fmap (fmap (listToMaybe . S.toList))
    $ m3

cancelArrow
  :: (Ord o)
  => Arrow o
  -> LinearCubeOfResolutions o
  -> LinearCubeOfResolutions o
cancelArrow arr = if source arr == target arr
  then error "source == targetR"
  else
    (`deleteObjectAt` target arr)
    . (`deleteObjectAt` source arr)
    . changeArrows2 arr
 where
  changeArrows2 a c = c { arrows = arrows c `addArrowsMod2` newArrows }
   where
    target'    = target a
    source'    = source a
    toTarget   = S.delete arr (arrowsTo c target')
    fromSource = S.delete arr (arrowsFrom c source')
    targetData = S.delete source' . S.map source $ toTarget -- get (sourceR, source) for everything which goes to target
    sourceData = S.delete target' . S.map target $ fromSource -- get (targetR, target) for everything from the source
    newArrows  = arrowsFromData targetData sourceData
    arrowsFromData
      :: (Ord o, Ord i)
      => Set (i, o)
      -> Set (i, o)
      -> Map i (Map o (Map i (Set o)))
    arrowsFromData s1 s2 =
      let l1 = S.toList s1
          l2 = S.toList s2
          ll = [ (i1, (o1, (i2, o2))) | (i1, o1) <- l1, (i2, o2) <- l2 ]
          makeMap :: (a, (b, (c, d))) -> Map a (Map b (Map c (Set d)))
          makeMap (x, (y, (z, w))) =
            M.singleton x (M.singleton y (M.singleton z (S.singleton w)))
      in  addArrowsMod2s . fmap makeMap $ ll



arrowsTo
  :: Ord o => LinearCubeOfResolutions o -> (Resolution, o) -> Set (Arrow o)
arrowsTo c (index, object) =
  S.fromList
    . fmap (\(r, o) -> (r, o, index, object))
    . concatMap sequence
    . M.toList
    . fmap
        ( M.keys
        . M.filter (S.member object)
        . fmap (M.! index) -- look at the target index.  would be more elegant to combine this and below
        . M.filter (M.member index)
        ) -- only look at keys which have something to the target index
    . M.filterWithKey (\r _ -> weight r < weight index)  -- only include stuff with lower index
    $ arrows c

arrowsFrom
  :: (Ord o) => LinearCubeOfResolutions o -> (Resolution, o) -> Set (Arrow o)
arrowsFrom c (index, object) =
  setMaybe
    .   fmap
          ( S.fromList
          . fmap (\(r', o') -> (index, object, r', o'))
          . concatMap (traverse S.toList)
          . M.toList
          . M.filter (not . S.null)
          )
    $   M.lookup index (arrows c)
    >>= M.lookup object
 where
  setMaybe :: Maybe (Set o) -> Set o
  setMaybe (Just os) = os
  setMaybe Nothing   = S.empty

deleteObjectAt
  :: (Ord o)
  => LinearCubeOfResolutions o
  -> (Resolution, o)
  -> LinearCubeOfResolutions o
deleteObjectAt cube (ind, obj) = LinearCubeOf
  { arrows = deleteTo (ind, obj) . deleteFrom (ind, obj) $ arrows cube
  }
 where
  deleteTo (ind', obj') = fmap (fmap (M.adjust (S.delete obj') ind'))
  deleteFrom (ind', obj') = M.adjust (M.delete obj') ind'

-- pruneMap' :: Foldable f => Map a (f b) -> Map a (f b)
-- pruneMap' = M.filter (not . F.null)

-- prune :: LinearCubeOfResolutions o -> LinearCubeOfResolutions o
-- prune c = c { arrows = pruneMap $ (arrows c) }
