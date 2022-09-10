{-|
Module      : Core.PlanarDiagram
Description : Planar diagrams of links
Copyright   : (c) Adam Saltz, 2020
License     : GPL-3
Maintainer  : saltz.adam@gmail.com
Stability   : experimental
Portability : POSIX

Planar diagrams of links and tangles ala [Knot Atlas](http://katlas.org/wiki/Planar_Diagrams).
-}
{-# LANGUAGE DeriveFoldable#-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module Core.PlanarDiagram
-- TODO: export list!
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

-- * Types

-- | A 'Join' connects two things, e.g. @Node Point@ is an edge in a flat diagram.  Nodes represent planar diagrams.  For diagrams with crossings, use 'CrossNode'.  Arguably not the best name.  
data Node a = Join a a deriving (Show, Generic,Read)
-- | Like a 'Node' but for diagrams with crossings.  For more on @Cross@ see the Knot Atlas link at the top of the page.
data CrossNode a = Cross CrossingType a a a a | CrossJoin a a
    deriving (Show, Read, F.Foldable)
-- | @CrossingType@ is used for positive and negative braid generators, not positive and negative crossings of an oriented link diagram.
data CrossingType = Pos | Neg deriving (Eq, Ord, Show, Read)



-- | Note that @CrossJoin x y == CrossJoin y x@.
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

-- | Decorations are arrows which connect pieces of a link diagram, see [Szabo](https://arxiv.org/abs/1010.4252).  Decorations extend between edges of a diagram, not points.

data Decoration' a = Decoration a a
deriving instance (Show a, Ord a) => Show (Decoration' a)
deriving instance Ord a => Ord (Decoration' a)
-- | Unlike 'Node', @Decoration x y != Decoration y x@
deriving instance Ord a => Eq (Decoration' a) 
type Decoration = Decoration' Arc

data Arc = Arc Point Point deriving (Eq,Show,Ord)

arcToPair :: Arc -> (Point, Point)
arcToPair (Arc p q) = (p,q)

type PD = AdjacencyMap Point

-- | There are two reasonable monoid instances for 'PD' because there are two instances for @AdjacencyMap@.  We choose the one based on +, i.e. on "overlay".  [LINK TO ADJACENCY MAP]
newtype PDConnectMonoid = PDCM PD deriving (Ord, Eq, Show)

-- wrapper for Monoid PD
-- (because there are two reasonable
--  monoid instances for PD!)
instance Semigroup PDConnectMonoid where
  (PDCM a) <> (PDCM b) = PDCM (a + b)
instance Monoid PDConnectMonoid where
  mempty = PDCM empty
  mappend = (<>)


type CrossPD = [CrossNode Point]

instance DisjointUnionable CrossPD
  where dUnion = dUnionDiag

-- | A @DecoratedResolvedDiagram@ is a @PD@ with some decorations.  Practically a staging ground for building a 'Configuration'.
data DecoratedResolvedDiagram = DRDiagram { drdDiagram :: PD
                                          , decorationsDR :: [Decoration]}
                       deriving (Eq, Show, Ord)




-- instance Eq a => Eq (Node a) where
--   (Join a b) == (Join x y) = (a == x && b == y) || (a == y && b == x)

-- instance  (Ord a, Hashable a) => Hashable (Node a) where
--   hashWithSalt n (Join x y) = hashWithSalt n (sort [x,y])

-- instance (Ord a) => Ord (Node a) where
-- --compare (Join a b) (Join c d) = compare (S.fromList [a,b]) (S.fromList [c,d])
--   compare (Join a b) (Join c d) = compare (maximum [a,b]) (maximum [c,d])

-- instance Functor Node where
--   fmap f (Join x y) = Join (f x) (f y)

-- * Getters, setters, converters, collectors

-- | Checks if a @CrossNode@ is a @CrossJoin@.
isCross :: CrossNode a -> Bool
isCross n = case n of
  Cross{}     -> True
  CrossJoin{} -> False


-- | Get the y-coordinates of the points conected by a 'Node'.
_yNode :: CrossNode Point -> [Int]
_yNode (CrossJoin p q  ) = _y <$> [p, q]
_yNode (Cross _ a b c d) = _y <$> [a, b, c, d]

-- | Get the y-coordinates of the points conected by a 'Node'.
_xNode :: CrossNode Point -> [Int]
_xNode (CrossJoin p q  ) = _x <$> [p, q]
_xNode (Cross _ a b c d) = _x <$> [a, b, c, d]


-- | @shiftNodeXBy 4@ shifts the x-coordinate of a @Node@ by 4.
shiftNodeXBy :: Int -> CrossNode Point -> CrossNode Point
shiftNodeXBy k = fmap (shiftXBy k)

-- | @shiftNodeYBy 4@ shifts the y-coordinate of a @Node@ by 4.
shiftNodeYBy :: Int -> CrossNode Point -> CrossNode Point
shiftNodeYBy k = fmap (shiftYBy k)

-- | The target of a 'Decoration'.
to :: Decoration' a -> a
to (Decoration _ y) = y

-- | The source of a 'Decoration'.
from :: Decoration' a -> a
from (Decoration x _) = x

-- | A decoration connects two edges.  @decPoints dec@ returns the four points connected by those edges.
decPoints :: Decoration' Arc -> [Point]
decPoints (Decoration (Arc x y) (Arc z w)) = [x,y,z,w]

-- TODO: bad function, replace
arcPoints :: Arc -> [Point]
arcPoints (Arc p q) = [p,q]

-- | Is the 'Arc' in the 'Component?
arcInComp :: Arc -> Component -> Bool
arcInComp (Arc x y) = bothMembers (x,y)

-- | Is the 'Arc' in the 'PD'?
arcInPD :: Arc -> PD -> Bool
arcInPD (Arc x y) pd = (x,y) `S.member` edgeSet pd || (y,x) `S.member` edgeSet pd

-- | Maps across all the points in a @CrossPD@.
onPoints :: (Point -> Point) -> CrossPD -> CrossPD
onPoints f = fmap (fmap f)

-- | Vertical disjoint union of @CrossPD@s.
dUnionDiag :: CrossPD -> CrossPD -> CrossPD
dUnionDiag cpd cpd' = cpd ++ shift cpd'
  where shift = onPoints (shiftYBy (pdBottom cpd + 1))

-- | Converts a 'CrossPD' into a 'PD'.  Returns an error if the @CrossPD@ has any crossings, i.e. 'Cross' constructors.
unsafeCrossToFlat :: CrossPD -> PD
unsafeCrossToFlat = overlays . concatMap parse
 where
  parse :: Ord a => CrossNode a -> [AdjacencyMap a]
  parse (CrossJoin a b  ) = [connect (vertex a) (vertex b)]
  parse Cross{} = error "not flat"

-- | Safe version of 'unsafeCrossToFlat'.
crossToFlat :: CrossPD -> Maybe PD
crossToFlat = fmap overlays . traverse aux -- should be simplifiable
 where
  aux (CrossJoin x y) = Just (connect (vertex x) (vertex y))
  aux Cross{}         = Nothing

-- | @flatToCross' (Join a b) = CrossJoin a b@.  You could use this to write @flatToCross :: PD -> CrossPD@ but I didn't.
flatToCross' :: Node a -> CrossNode a
flatToCross' (Join a b) = CrossJoin a b

-- flatToCross :: PD -> CrossPD
-- flatToCross = fmap aux where aux (Join a b) = CrossJoin a b

-- | The points in a component of a diagram.  For oriented diagrams, assumed to be a strongly-connected component.
type Component = S.Set Point -- assumed to be strongly-connected component


-- | All the points in an adjacency map.
getPoints :: AdjacencyMap Point -> [Point]
getPoints = S.toList . vertexSet


-- | Collects all the points in a @[CrossNode Point]@, i.e. a @CrossPD@.
getPointsC :: CrossPD -> [Point]
getPointsC xs = getPointsC' xs []
--write as foldl'!
getPointsC' :: [CrossNode Point] -> [Point] -> [Point]
getPointsC' (CrossJoin a b   : ns) as = getPointsC' ns (a : b : as)
getPointsC' (Cross _ a b c d : ns) as = getPointsC' ns (a : b : c : d : as)
getPointsC' []                     as = as

-- assumes PD is oriented
-- | @searchPD f pd@ returns all connected components of @pd@ which contain a point which satisfies @f@.  For example, you could search for all components which meet a particular 'Decoration'.  This function assumes that the @PD@ is oriented.
searchPD :: (Point -> Bool) -> PD -> PD
searchPD f pd = let
  found = S.filter f (vertexSet pd)
  reachableFrom found' = F.fold (S.map (S.fromList . (`reachable` pd)) found')
  reachableTo found' = F.fold (S.map (S.fromList . (`reachable` (Algebra.Graph.AdjacencyMap.transpose pd))) found')
  reachableToAndFrom found' = reachableTo found' `S.union` reachableFrom found'
  in
    induce (`S.member` reachableToAndFrom found) pd


-- does not assume that PD is oriented
-- | Same as 'searchPD' but without assuming that the @PD@ is oriented.  Slower!
searchPDUnO :: (Point -> Bool) -> PD -> PD
searchPDUnO f pd = let
  found = filter f (vertexList pd)
  reachableToAndFrom found' = S.fromList (concatMap (`reachable` symmetricClosure pd) found')
  in
    induce (`S.member` reachableToAndFrom found) pd


-- | Returns the highest y-coordinate in a 'CrossPD'.  Using the convention on 'Core.Grid', this gets the y-coordinate of the *lowest* point.
pdBottom :: CrossPD -> Int
pdBottom t = if null t then 0 else maximum . (_yNode =<<) $ t -- lol

-- | Returns the highest x-coordinate in a 'CrossPD'.  Using the convention on 'Core.Grid', this gets the x-coordinate of the *rightmost* point.
pdWidth :: CrossPD -> Int
pdWidth t = if null t then 0 else maximum . (_xNode =<<) $ t

-- | Flip a @CrossPD@ vertically.
upsideDown :: CrossPD -> CrossPD
upsideDown pd =
  shiftNodeYBy maxY . shiftNodeXBy maxX . fmap negatePoint <$> pd
 where
  maxY = maximum . fmap _y . getPointsC $ pd
  maxX = maximum . fmap _x . getPointsC $ pd

-- | @nestingLevel pd comp@ returns the number of components which contain (i.e. enclose) @comp@ in @pd@.  Important for some orientation calculations and so on.
nestingLevel :: PD -> Component -> Int
nestingLevel pd comp = --let comps = pdComponents pd in
                         if null comp
                         then 0
                         else
                           let p = head . S.toList $ comp
                           in
                             S.size . S.filter (\p' -> _x p' > _x p && _y p' == _y p) $ vertexSet pd

-- * Resolutions

-- | Returns the names (i.e. sequence of 0s and 1s) of all resolutions of a 'CrossPD'.
allRes :: CrossPD -> [Resolution]
allRes pd = sequence binary
  where binary = replicate (length $ filter isCross pd) [0, 1]


-- a b
-- c d 
-- | Constructs a 'PD' by resolving 'CrossPD' according to 'Resolution'.  The crossings are ordered by their position in the list @CrossPD@.  This works great for 'Braids' because they have a natural ordering already.
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

-- | Constructs a 'DecoratedResolvedDiagram' by resolving 'CrossPD' according to 'Resolution'.  Like 'resolvePD' but has to keep track of the decorations.
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

-- | Strongly-connected components of a @PD@.
pdComponents :: PD -> [Component]
pdComponents = fmap NEAM.vertexSet . pdComponents'

pdComponents' :: Ord a => AdjacencyMap a -> [NEAM.AdjacencyMap a]
pdComponents' = S.toList . vertexSet . scc . symmetricClosure

-- | Returns a @Map@ from 'Resolution' to the resolved 'PD'. 
cubeOfResolutions :: CrossPD -> M.Map Resolution PD
cubeOfResolutions pd =
  M.fromList [ (res, resolvePD pd res) | res <- allRes pd ]

-- | Returns a @Map@ from 'Resolution' to the resolved 'DecoratedResolvedDiagram'. 
cubeOfResolutionsDecorated
  :: CrossPD -> M.Map Resolution DecoratedResolvedDiagram
cubeOfResolutionsDecorated cpd = graphMap (resolveToDRD cpd) (allRes cpd)
-- IMPORTANT : grid has -> x
--                       V y
-- axes!  because braid starts at top                       

-- check that all the edges have included endpoints, then throw out the points

-- * Drawing

-- | Reads a 'DecoratedResolvedDiagram' from a 'GridPic'.
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


