{-# LANGUAGE ScopedTypeVariables #-}
module Core.Util 
(bothMembers,
 fromNonEmptyGraph,
 fromNonEmptyAM,
    alterAt,
    toEdges,
    replaceAt,
    uncurry3,
    fst3,
    snd3,thd3, zipWithTuple3, zipWithTuple2,
    swap,
    replaceBy,
    replaceFirstBy,
    compose,
    kleisliCompose,
    deleteAt,
    deleteAtPair,
    partitionBy,
    mapTuple,
    getAdjacentCyclic,
    isCycleOf,
    cyclicPermutations,
    cartesianProduct,
    cartesianProducts,
    xor,
    required,
    vacuous,
    maybeToInt,
    listToTuple,
    graph,
    graphF,
    graphMap,
    const2,
    pairs,
    other,
    nonemptySubsets,
    consumeNestedStep,
    bigNestedToList,
    listToSetMap,
    listToSetMod2,
    takeTil,
    combineMapsWithFilter,
    insertAt,
    graphMapPar
)
where
import Data.List ((\\), find,sort)
import Control.Monad (join,(<=<))
import Control.Arrow ((***),first)
import Data.Map.Lazy (Map)
import Data.Set (Set)
import qualified Data.Set as S 
import qualified Data.Map.Lazy as M
import qualified Algebra.Graph.NonEmpty as NE
import qualified Algebra.Graph as G
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NEAM

import Control.Parallel.Strategies

-- partition list of a's by how they match with b's
partitionBy :: Eq a => (b -> a -> Bool) -> [b] -> [a] -> [[a]]
partitionBy f bs as = filter (/= []) $ partitionByAccum f bs as []

partitionByAccum :: Eq a => (b -> a -> Bool) -> [b] -> [a] -> [[a]] -> [[a]]
partitionByAccum f (b:bs) as accum  = partitionByAccum f bs as' (bit:accum)
            where
                bit = filter (f b) as
                as' = as \\ bit
partitionByAccum _ _ _ accum = accum

{-partitionSetBy :: Ord a => (b -> a -> Bool) -> [b] -> Set a -> [Set a]
partitionSetBy _ [] _      = []
partitionSetBy f (b:bs) as = bit : partitionSetBy f bs as'
            where
                bit = Set.filter (f b) as
                as' = as `Set.difference` bit
-}
-- replace x by y
replaceBy :: Eq a => a -> a -> [a] -> [a]
replaceBy x y as = (\s -> if s == x then y else s) <$> as

replaceFirstBy :: Eq a => a -> a -> [a] -> [a]
replaceFirstBy _ _ [] = []
replaceFirstBy x y (a:as) = if x == a then y:as 
                            else a:replaceFirstBy x y as

insertAt :: a -> Int -> [a] -> [a]
insertAt  = go  where
  go :: a -> Int -> [a] -> [a]
  go x' 0 xs' = x':xs'
  go x' i' (y:xs') = y:go x' (i'-1) xs'
  go x' _ [] = [x']

{-replaceBySet :: Ord a => a -> a -> Set a -> Set a
replaceBySet x y = Set.map (\s -> if s == x then y else s)
-}
-- ala haskell wiki's 99 problems
-- data Edge a = Edge a a
-- type Graph a = ([a], [Edge a])

-- depthFirst :: Eq a => Graph a -> a -> [a]
-- depthFirst (v,e) a 
--     | null [x | x<-v, x == a]    = []
--     | otherwise                  = dfrecursive (v,e) [a]

-- dfrecursive :: Eq a => Graph a -> [a] -> [a]
-- dfrecursive ([],_) _ = []
-- dfrecursive (_,_) [] = []
-- dfrecursive (v,e) (a:as)
--     | null [x | x<-v, x == a]    = dfrecursive (newv, e) as
--     | otherwise                  = a : dfrecursive (newv, e) (adjacent ++ as)
--     where 
--         adjacent = [x | (Edge x y) <- e, y == a] ++ [x | (Edge y x) <- e, y == a]
--         newv = [x | x <- v, x /= a]

-- connectedComponents :: Eq a => Graph a -> [[a]]
-- connectedComponents ([],_) = []
-- connectedComponents (a:as, e) 
--     | null remaining = [connected]
--     | otherwise      = connected : connectedComponents (remaining, e)
--     where
--         connected = depthFirst (a:as, e) a
--         remaining = (a:as) \\ connected

cyclicPermutations :: [a] -> [[a]]
cyclicPermutations xs = take (length xs) (iterate shift xs) where
    shift :: [a] -> [a]
    shift (x:xs') = xs' ++ [x]
    shift []     = []  -- pattern match

isCycleOf :: Eq a => [a] -> [a] -> Bool
as `isCycleOf` bs = as `elem` cyclicPermutations bs

{-
-- FOR A TRANSITIVE RELATIONSHIP JUST USE ZIPWITH
-- e.g. zipWith f xs (tail xs)
checkPairwise :: (a -> a -> Bool) -> [a] -> Bool
checkPairwise f (a:as) = getAll (go f (a:as)) where 
                            go f' (a':as') = foldMap (All . f' a') as' <> go f' as'
                            go _ [] = All True
checkPairwise _ []     = True -- pattern match - correct?
-}
xor :: Bool -> Bool -> Bool
xor a b = (a || b) && (not a || not b)

-- bad for short lists
getAdjacentCyclic :: Eq a => a -> [a] -> Maybe [a]
getAdjacentCyclic a as = case find (== a) as of
    Nothing -> Nothing
    Just _ -> Just $ getAdjacent a (cycle as)
    where
        getAdjacent a' (b':c':d':as') = if c' == a' then [b',a',d']
                                        else getAdjacent a' (c':d':as')
        getAdjacent _ _ = [] -- cannot attain -- cycle gives an infinite list

alterAt :: Eq a => Int -> (a -> a) -> [a] -> [a]
alterAt 0 f (a:as) = f a : as
alterAt n f (a:as) = a : alterAt (n - 1) f as
alterAt _ _ [] = []

replaceAt :: Eq b => Int -> b -> [b] -> [b]
replaceAt n b = alterAt n (const b)

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

-- should be <=< rather than >=>
-- <=< works like .
-- >=> works like &
kleisliCompose :: Monad m => [a -> m a] -> a -> m a
kleisliCompose = foldr (<=<) return

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) ->d
uncurry3 f (a,b,c) = f a b c

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

zipWithTuple3 :: (a -> a -> a) -> (a,a,a) -> (a,a,a) -> (a,a,a)
zipWithTuple3 f (s,t,u) (v,w,y) = (f s v, f t w, f u y)

zipWithTuple2 :: (a -> a -> a) -> (a,a) -> (a,a) -> (a,a)
zipWithTuple2 f (s,t) (v,w) = (f s v, f t w)


mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple = join (***)

swap ::(a,b) -> (b,a)
swap (a,b) = (b,a)

required :: Maybe Bool -> Bool
required (Just x) = x
required Nothing = False

vacuous :: Maybe Bool -> Bool
vacuous (Just x) = x
vacuous Nothing = True

maybeToInt :: Maybe Int -> Int
maybeToInt (Just c) = c
maybeToInt Nothing = 0

listToTuple :: Show a => [a] -> (a,a)
listToTuple [x,y] = (x,y)
listToTuple a = error $ "told you not to use this " ++ show a

graph :: (a -> b) -> a -> (a,b)
graph f x = (x, f x)

graphF :: Functor f => (a -> b) -> f a -> f (a,b)
graphF f = fmap (graph f)

graphFPar :: (k -> a) -> [k] -> [(k,a)]
graphFPar f xs = fmap (graph f) xs `using` parList rseq

graphMap :: (Ord a) => (a -> b) -> [a] -> Map a b
graphMap f = M.fromList . graphF f

graphMapPar :: Ord k => (k -> a) -> [k] -> Map k a
graphMapPar f = M.fromList . graphFPar f


const2 :: a -> b -> c -> a
const2 a _ _ = a

toEdges :: [a] -> [(a,a)]
toEdges xs = zip xs (tail xs)

pairs :: [a] -> [(a,a)]
pairs (x:xs) = [(x,y) | y <- xs] ++ pairs xs
pairs [] = []

other :: Eq a => a -> (a,a) -> a
other x (y,z) | y == x = z
              | z == x = y
              | otherwise = y


deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt i (x:xs) | i == 0 = xs
                  | otherwise = x : deleteAt (i-1) xs


deleteAtPair :: (Int,Int) -> [a] -> [a]
deleteAtPair _ [] = []
deleteAtPair (i,j) (x:xs) = if i > j then deleteAtPair (j,i) (x:xs)
                                     else
                                       deleteAtPair' (i,j) (x:xs)
                                       where
                                         deleteAtPair' (_,_) [] = []
                                         deleteAtPair' (_,0) (x':xs') = x':xs'
                                         deleteAtPair' (0,j') (_:xs') = deleteAtPair (-1,j'-1) xs'
                                         deleteAtPair' (i',j') (x':xs') = x' : deleteAtPair (i'-1,j'-1) xs'
--nonemptySubsequences :: [a] -> [[a]]
--nonemptySubsequences = filter (not . null) . subsequences

nonemptySubsets :: (Ord a) => Set a -> [Set a]
nonemptySubsets = S.toList . S.delete S.empty . S.foldl' (slift S.union) (S.singleton S.empty) . S.map makeSet
  where
  slift f s s' = unionAll (S.map (\e -> S.map (f e) s') s)
  makeSet :: (Ord a) => a -> Set (Set a)
  makeSet = S.insert S.empty . S.singleton . S.singleton
  unionAll = S.foldl' S.union S.empty

cartesianProduct :: (Ord a, Ord b) => Set a -> Set b -> Set (a,b)
cartesianProduct as bs = S.fromList [(x,y) | x <- S.toList as, y <- S.toList bs]

cartesianProducts :: Ord a => [Set a] -> Set [a]
cartesianProducts = S.fromList . traverse S.toList

bigNestedToList :: Map a (Map a (Map b (Set b))) -> [(a,a,b,b)]
bigNestedToList = fmap (\(a,(b,(c,d))) -> (a,b,c,d)) . concatMap sequence . fmap (fmap (concatMap sequence . fmap (fmap (concatMap sequence)))) . M.toList . fmap (M.toList . fmap (M.toList . fmap S.toList))

consumeNestedStep :: (Ord d) => (a -> b -> d) -> Map a (Map b c) -> Map d c
consumeNestedStep f =  M.fromList . concatMap (\(g, l) -> fmap (first g) l) . fmap (fmap M.toList . first f) . M.toList

listToSetMap :: (Ord a) => [(a,a)] -> Map a (Set a)
listToSetMap = fmap listToSetMod2 . M.fromListWith (++) . fmap (fmap (:[]))

listToSetMod2 :: (Ord a) => [a] -> Set a
listToSetMod2 xs = S.fromList . prune $ sort xs where
  prune (x:y:xs') = if x == y then prune xs else x : prune (y:xs')
  prune [x] = [x]
  prune [] = []

-- first element of second item is x
-- i.e. 
takeTil :: Eq a => [a] -> a -> ([a],[a])
takeTil xs y = go xs y ( [],[]) where
  go (x':xs') y' (as,bs) = if x' == y' then (as,(x':xs') ++ bs)
                           else go xs' y' (as ++ [x'],bs)
  go [] _ (as,bs) = (as,bs)

combineMapsWithFilter :: (Ord i) => (i -> i -> Bool) -> (i -> i -> i) -> (o -> o -> o) -> Map i o -> Map i o -> Map i o
combineMapsWithFilter f combineI combineO m m' = M.fromList [(combineI r s, combineO x y) | (r,x) <- M.toList m, (s,y) <- M.toList m', f r s]

-- mapKeys :: (Ord k, Ord k', Eq k') => (k -> k') -> Map k o -> Map k' o
-- mapKeys f = M.fromList . M.foldrWithKey (\key x xs -> (f $! key, x) : xs) []
 
bothMembers :: Ord a => (a,a) -> Set a -> Bool
(x,y) `bothMembers` zs = y `S.member` zs && x `S.member` zs

fromNonEmptyGraph :: NE.Graph a -> G.Graph a
fromNonEmptyGraph (NE.Vertex a) = G.Vertex a
fromNonEmptyGraph (NE.Connect a b) = G.Connect (fromNonEmptyGraph a) (fromNonEmptyGraph b)
fromNonEmptyGraph (NE.Overlay a b) = G.Overlay (fromNonEmptyGraph a) (fromNonEmptyGraph b)

fromNonEmptyAM :: Ord a => NEAM.AdjacencyMap a -> AM.AdjacencyMap a
fromNonEmptyAM neam = AM.overlay (AM.overlays . fmap AM.vertex . S.toList .  NEAM.vertexSet $ neam) . AM.fromAdjacencySets . M.toList . M.fromListWith S.union . fmap (fmap S.singleton) . NEAM.edgeList $ neam
                  
