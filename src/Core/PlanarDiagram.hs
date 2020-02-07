{-# LANGUAGE DeriveFoldable#-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module Core.PlanarDiagram
where

import qualified Data.Foldable                 as F
import qualified Data.Map.Lazy               as M
import qualified Data.Set                      as S
import           Data.Monoid (Monoid)                
import Data.Semigroup ((<>), Semigroup)

import Core.PlanarDiagram.Operations
import           Core.Resolution
import           Core.Util
import           Core.Grid
import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm 
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NEAM
import           GHC.Generics                   ( Generic )
import           Control.Arrow


data CrossingType = Pos | Neg deriving (Eq, Ord, Show, Read)

data Node a = Join a a deriving (Show, Generic,Read)


data CrossNode a = Cross CrossingType a a a a | CrossJoin a a
    deriving (Show, Read, F.Foldable)

shiftNodeXBy :: Int -> CrossNode Point -> CrossNode Point
shiftNodeXBy k = fmap (shiftXBy k)

shiftNodeYBy :: Int -> CrossNode Point -> CrossNode Point
shiftNodeYBy k = fmap (shiftYBy k)

instance (Ord a) => Eq (CrossNode a) where
    CrossJoin{} == Cross{} = False
    Cross{} == CrossJoin{} = False
    (CrossJoin a b) == (CrossJoin c d) = (a == c && b == d) || (a == d && b == c)
    (Cross t a b c d) == (Cross t' e f g h) = (S.fromList [a,b,c,d] == S.fromList [e,f,g,h]) && t == t'

instance (Ord a) => Ord (CrossNode a) where
  compare CrossJoin{} Cross{} = LT
  compare Cross{} CrossJoin{} = GT
  compare (CrossJoin a b) (CrossJoin c d) = compare (S.fromList [a,b]) (S.fromList [c,d])
  compare (Cross _ a b c d) (Cross _ e f g h) = compare (S.fromList [a,b,c,d]) (S.fromList [e,f,g,h])

instance Functor CrossNode where
    fmap f (Cross t a b c d) = Cross t (f a) (f b) (f c) (f d)
    fmap f (CrossJoin a b)        = CrossJoin (f a) (f b)

-- instance Eq a => Eq (Node a) where
--   (Join a b) == (Join x y) = (a == x && b == y) || (a == y && b == x)

-- instance  (Ord a, Hashable a) => Hashable (Node a) where
--   hashWithSalt n (Join x y) = hashWithSalt n (sort [x,y])

-- instance (Ord a) => Ord (Node a) where
-- --compare (Join a b) (Join c d) = compare (S.fromList [a,b]) (S.fromList [c,d])
--   compare (Join a b) (Join c d) = compare (maximum [a,b]) (maximum [c,d])

-- instance Functor Node where
--   fmap f (Join x y) = Join (f x) (f y)

data Arc = Arc Point Point deriving (Eq,Show,Ord)

arcToPair :: Arc -> (Point, Point)
arcToPair (Arc p q) = (p,q)

data Decoration' a = Decoration a a
deriving instance (Show a, Ord a) => Show (Decoration' a)
deriving instance Ord a => Ord (Decoration' a)
deriving instance Ord a => Eq (Decoration' a)
type Decoration = Decoration' Arc

to :: Decoration' a -> a
to (Decoration _ y) = y

from :: Decoration' a -> a
from (Decoration x _) = x

-- TODO: bad function, replace
arcPoints :: Arc -> [Point]
arcPoints (Arc p q) = [p,q]

decPoints :: Decoration' Arc -> [Point]
decPoints (Decoration (Arc x y) (Arc z w)) = [x,y,z,w]

-- flipNode :: Node a -> Node a
-- flipNode (Join a b) = Join b a

_yNode :: CrossNode Point -> [Int]
_yNode (CrossJoin p q  ) = _y <$> [p, q]
_yNode (Cross _ a b c d) = _y <$> [a, b, c, d]

_xNode :: CrossNode Point -> [Int]
_xNode (CrossJoin p q  ) = _x <$> [p, q]
_xNode (Cross _ a b c d) = _x <$> [a, b, c, d]


type PD = AdjacencyMap Point

type CrossPD = [CrossNode Point]

onPoints :: (Point -> Point) -> CrossPD -> CrossPD
onPoints f = fmap (fmap f)

instance DisjointUnionable CrossPD
  where dUnion = dUnionDiag

dUnionDiag :: CrossPD -> CrossPD -> CrossPD
dUnionDiag cpd cpd' = cpd ++ shift cpd'
  where shift = onPoints (shiftYBy (pdBottom cpd + 1))


unsafeCrossToFlat :: CrossPD -> PD
unsafeCrossToFlat = overlays . concatMap parse
 where
  parse :: Ord a => CrossNode a -> [AdjacencyMap a]
  parse (CrossJoin a b  ) = [connect (vertex a) (vertex b)]
  parse Cross{} = error "not flat"

crossToFlat :: CrossPD -> Maybe PD
crossToFlat = fmap overlays . traverse aux -- should be simplifiable
 where
  aux (CrossJoin x y) = Just (connect (vertex x) (vertex y))
  aux Cross{}         = Nothing

flatToCross' :: Node a -> CrossNode a
flatToCross' (Join a b) = CrossJoin a b

-- flatToCross :: PD -> CrossPD
-- flatToCross = fmap aux where aux (Join a b) = CrossJoin a b

type Component = S.Set Point -- assumed to be strongly-connected component

getPoints :: AdjacencyMap Point -> [Point]
getPoints = S.toList . vertexSet


--write as foldl'!
getPointsC' :: [CrossNode Point] -> [Point] -> [Point]
getPointsC' (CrossJoin a b   : ns) as = getPointsC' ns (a : b : as)
getPointsC' (Cross _ a b c d : ns) as = getPointsC' ns (a : b : c : d : as)
getPointsC' []                     as = as

getPointsC :: CrossPD -> [Point]
getPointsC xs = getPointsC' xs []

pdBottom :: CrossPD -> Int
pdBottom t = if null t then 0 else maximum . (_yNode =<<) $ t -- lol

pdWidth :: CrossPD -> Int
pdWidth t = if null t then 0 else maximum . (_xNode =<<) $ t

upsideDown :: CrossPD -> CrossPD
upsideDown pd =
  shiftNodeYBy maxY . shiftNodeXBy maxX . fmap negatePoint <$> pd
 where
  maxY = maximum . fmap _y . getPointsC $ pd
  maxX = maximum . fmap _x . getPointsC $ pd
-- sliceAt (Comp comp) p p' = Just (p...,p'...)
-- sliceAt2
--   :: Component -> Node Point -> Node Point -> Maybe (Component, Component)
-- sliceAt2 (Comp comp) p p' = if (p `elem` comp) && (p' `elem` comp)
--   then if (fromJust (elemIndex p comp)) < (fromJust (elemIndex p' comp))
--     then
--       Just
--       . (\(a, (b, c)) -> (Comp b, Comp (c ++ a)))
--       . second (`takeTil` p')
--       $ (comp `takeTil` p)
--     else
--       Just
--       . (\(a, (b, c)) -> (Comp (c ++ a), Comp b))
--       . second (`takeTil` p)
--       $ (comp `takeTil` p')
--   else Nothing

-- sliceAt :: Component -> Node Point -> Maybe Component
-- sliceAt (Comp comp) p = if p `notElem` comp
--   then Nothing
--   else Just . Comp $ uncurry (++) (swap (takeTil comp p))

-- type MarkedPoint = Int -- no

-- switched from data

-- why isn't this just configuration??


data DecoratedResolvedDiagram = DRDiagram { drdDiagram :: PD
                                          , decorationsDR :: [Decoration]}
                       deriving (Eq, Show, Ord)

isCross :: CrossNode a -> Bool
isCross n = case n of
  Cross{}     -> True
  CrossJoin{} -> False

allRes :: CrossPD -> [Resolution]
allRes pd = sequence binary
  where binary = replicate (length $ filter isCross pd) [0, 1]


-- a b
-- c d 
resolvePD :: CrossPD -> Resolution -> PD
resolvePD (CrossJoin a b : ns) res = connect (vertex a) (vertex b) + resolvePD ns res
resolvePD (Cross Pos a b c d : ns) (r : res)
  | r == 0 = connect (vertex a) (vertex c) + connect (vertex b) (vertex d) + resolvePD ns res
  | r == 1 = connect (vertex a) (vertex b) + connect (vertex c) (vertex d) + resolvePD ns res
resolvePD (Cross Neg a b c d : ns) (r : res)
  | r == 0 = connect (vertex a) (vertex b) + connect (vertex c) (vertex d) + resolvePD ns res
  | r == 1 = connect (vertex a) (vertex c) + connect (vertex b) (vertex d) + resolvePD ns res    
resolvePD (Cross{} : _) [] = error "more crossings than resolutions?"
resolvePD [] _  = empty

newtype PDConnectMonoid = PDCM PD deriving (Ord, Eq, Show)

-- wrapper for Monoid PD
-- (because there are two reasonable
--  monoid instances for PD!)
instance Semigroup PDConnectMonoid where
  (PDCM a) <> (PDCM b) = PDCM (a + b)
instance Monoid PDConnectMonoid where
  mempty = PDCM empty
  mappend = (<>)


resolveToDRD :: CrossPD -> Resolution -> DecoratedResolvedDiagram
resolveToDRD pd res = DRDiagram pd' decos
 where
  keepTrack :: CrossPD -> Resolution -> (PDConnectMonoid, [Decoration])
  keepTrack (CrossJoin a b : ns) res' = (PDCM (connect (vertex a) (vertex b)), []) <> keepTrack ns res'
  keepTrack (Cross Pos a b c d : ns) (r : res')
    | r == 0
    = (PDCM (connect (vertex a) (vertex c) + connect (vertex b) (vertex d)), [Decoration (Arc a c) (Arc b d)])
      <> keepTrack ns res'
    | r == 1
    = (PDCM (connect (vertex a) (vertex b)  + connect  (vertex c) (vertex d)), [Decoration (Arc a b) (Arc c d)])
      <> keepTrack ns res'
  keepTrack (Cross Neg a b c d : ns) (r : res')
    | r == 0
    = (PDCM (connect (vertex a) (vertex b)  + connect  (vertex c) (vertex d)), [Decoration (Arc a b) (Arc c d)])
      <> keepTrack ns res'
    | r == 1
    = (PDCM (connect (vertex a) (vertex c) + connect (vertex b) (vertex d)), [Decoration (Arc a c) (Arc b d)])
      <> keepTrack ns res'
  keepTrack [] _  = (PDCM empty, [])
  keepTrack _  [] = (PDCM empty, [])
  keepTrack _  _  = (PDCM empty, [])
  (PDCM pd', decos) = keepTrack pd res


pdComponents' :: Ord a => AdjacencyMap a -> [NEAM.AdjacencyMap a]
pdComponents' = S.toList . vertexSet . scc . symmetricClosure

pdComponents :: PD -> [Component]
pdComponents = fmap NEAM.vertexSet . pdComponents'

-- | Take a 'PD' and returns a list of all the 'Diagram's of its resolutions.
cubeOfResolutions :: CrossPD -> M.Map Resolution PD
cubeOfResolutions pd =
  M.fromList [ (res, resolvePD pd res) | res <- allRes pd ]

cubeOfResolutionsDecorated
  :: CrossPD -> M.Map Resolution DecoratedResolvedDiagram
cubeOfResolutionsDecorated cpd = graphMap (resolveToDRD cpd) (allRes cpd)
-- IMPORTANT : grid has -> x
--                       V y
-- axes!  because braid starts at top                       

-- check that all the edges have included endpoints, then throw out the points
gridPicToDRD :: GridPic -> DecoratedResolvedDiagram
gridPicToDRD gp@(GridPic ps es ds) = if not (isDiagram gp)
  then error "not a real diagram"
  else DRDiagram diagramNodes diagramDecos
 where
  -- diagramNodes :: PD
  diagramNodes = foldr (+) empty . fmap (uncurry connect .  (vertex *** vertex)) $ es
  diagramDecos = fmap (uncurry Decoration . (uncurry Arc *** uncurry Arc)) ds
  isDiagram :: GridPic -> Bool
  isDiagram gp'@(GridPic _ es' ds') =
    all (hasEndPoints gp') es' && all (intersectsAnEdge gp') ds'

  hasEndPoints :: GridPic -> (Point, Point) -> Bool
  hasEndPoints (GridPic ps'' _ _) (p, q) = p `elem` ps && q `elem` ps''

  intersectsAnEdge :: GridPic -> (Edge, Edge) -> Bool
  intersectsAnEdge (GridPic _ es'' _) (e, f) = e `elem` es'' && f `elem` es''

arcInComp :: Arc -> Component -> Bool
arcInComp (Arc x y) = bothMembers (x,y)

arcInPD :: Arc -> PD -> Bool
arcInPD (Arc x y) pd = (x,y) `S.member` edgeSet pd || (y,x) `S.member` edgeSet pd

--buildCubicalComplex :: PD -> CubeOf (Resolution,Resolution) DecoratedResolvedDiagram
-- buildCubicalComplex pd = resolvePD pd (allZeroes pd) where
--     allZeroes :: PD -> Resolution
--     allZeroes pd = replicate (length $ filter isCross pd) 0


-- does not assume that PD is oriented
searchPDUnO :: (Point -> Bool) -> PD -> PD
searchPDUnO f pd = let
  found = filter f (vertexList pd)
  reachableToAndFrom found' = S.fromList (concatMap (`reachable` symmetricClosure pd) found')
  in
    induce (`S.member` reachableToAndFrom found) pd

-- assumes PD is oriented
searchPD :: (Point -> Bool) -> PD -> PD
searchPD f pd = let
  found = S.filter f (vertexSet pd)
  reachableFrom found' = F.fold (S.map (S.fromList . (`reachable` pd)) found')
  reachableTo found' = F.fold (S.map (S.fromList . (`reachable` (Algebra.Graph.AdjacencyMap.transpose pd))) found')
  reachableToAndFrom found' = reachableTo found' `S.union` reachableFrom found'
  in
    induce (`S.member` reachableToAndFrom found) pd

nestingLevel :: PD -> Component -> Int
nestingLevel pd comp = --let comps = pdComponents pd in
                         if null comp
                         then 0
                         else
                           let p = head . S.toList $ comp
                           in
                             S.size . S.filter (\p' -> _x p' > _x p && _y p' == _y p) $ vertexSet pd
                           -- in  if _x p == _x p' -- it's vertical
                           -- then length $ filter
                           -- (\(Join q q') ->
                           --    (_x q == _x q')
                           --   && (_x q > _x p)
                           --   && ((_y q == _y p' && _y q' == _y p) || (_y q == _y p && _y q' == _y p'))
                           -- )
                           -- (comp `delete` comps) -- is vertical & is to the right of Join p p' & is on the same level as Join p p'
                           -- else length $ filter
                           -- (\(Join q q') ->
                           --    (_y q == _y q')
                           --   && (_y q > _y p)
                           --   && ((_x q == _x p' && _x q' == _x p) || (_x q == _x p && _x q' == _x p'))
                           -- )
                           -- (comp `delete` comps) -- is horizontal & is below Join p p' & is on the same level as Join p p'
