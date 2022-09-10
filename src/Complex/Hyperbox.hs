{-|
Module      : Complex.Hyperbox
Description : Working with hyperboxes
Copyright   : (c) Adam Saltz, 2020
License     : GPL-3
Maintainer  : saltz.adam@gmail.com
Stability   : experimental
Portability : POSIX

Implements hyperboxes, a linear-algebraic (or homological-algebraic?) device due to Manolescu and Ozsvath (LINK).  These aren't necessary for computing link homology theories, but are for working with bridge trisections.
-}
module Complex.Hyperbox where
import           Algebra.Z2
import           Data.Map.Lazy ( Map )
import qualified Data.Map.Lazy                as M
import           Data.Tuple.Extra
import           Data.List.Extra                ( unfoldr )
import           Data.Maybe                     ( catMaybes )
import           Core.Util                      ( compose
                                                , replaceAt
                                                , graphMap,
                                                  graphMapPar
                                                )
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Core.Cube
import           Core.Resolution
import           Algebra.V
import           Control.Applicative            ( liftA2 )

-- * Hyperboxes

-- | 'Delta' marks coordinates, 'Epsilon' represents steps from one coordinate to another.
type Delta = [Int]
type Epsilon = [Z2]

-- | Move from one 'Delta' to another via 'Epsilon'.
addDE :: Delta -> Epsilon -> Delta
d `addDE` e = zipWith (+) d (fmap z2ToInt e)

-- | A hyperbox with objects of type @object@ and morphisms of type @morphism@.  The @vertices@ are the underlying vector space.  @arrowsH@ is the self-map.
data Hyperbox object morphism = Hyperbox {vertices :: Map Delta object,
                                            arrowsH :: Map (Delta, Epsilon) morphism} deriving (Eq, Ord, Show)

-- | Build a hyperbox.
buildHyperbox
  :: (Delta -> object) -- ^ builds the objects
  -> ((Delta, Epsilon) -> morphism) -- ^ builds the morphisms
  -> Delta -- ^ size
  -> Hyperbox object morphism
buildHyperbox f g d = Hyperbox (graphMapPar f (upTo d)) (graphMapPar g pog)
 where
  emax = replicate (length d) ZOne
  pog =
    filter (not . all (== ZZero) . snd)
      [ (d', e') | d' <- upTo d, e' <- upToE emax, allLTE (d' `addDE` e') d ]
  allLTE (x : xs) (y : ys) = x <= y && allLTE xs ys
  allLTE []       []       = True
  allLTE _        _        = False

-- By "instantiate" I mean "convert @Labeling -> Set Labeling@ into a finite function so we can work with it."
instantiateH :: Hyperbox (Set Labeling) (Labeling -> Set Labeling)-> Hyperbox (Set Labeling) (Map Labeling (Set Labeling))
instantiateH hyperbox =  hyperbox {arrowsH = M.mapWithKey (\(d,_) f -> graphMap f (S.toList $ vertices hyperbox M.! d)) . arrowsH $ hyperbox}

-- * Compression
-- | In some sense, this function is the goal of the module.  We want to build hyperboxes, compress them, and get the morphism across the "long diagonal."
diagonalMap :: Ring m => Hyperbox o m -> m
diagonalMap h = (M.! (dd, ee)) $ arrowsH (compress h)
 where
  l  = length . head . M.keys . vertices $ h
  dd = replicate l 0
  ee = replicate l ZOne

-- | Specializes 'diagonalMap' but returns @o -> Set o@.
diagonalMapSet :: Ord o => Hyperbox (Set o) (Map o (Set o)) -> (o -> Set o)
diagonalMapSet h = \object ->
  maybeSet . M.lookup object . (M.! (dd, ee)) $ arrowsH (compress h)
 where
  l  = length . head . M.keys . vertices $ h
  dd = replicate l 1
  ee = replicate l ZOne
  maybeSet :: Maybe (Set a) -> Set a
  maybeSet (Just xs) = xs
  maybeSet Nothing   = S.empty

-- | Compress a hyperbox.  The remaining functions are helpers to 'compress'.
compress :: Ring m => Hyperbox o m -> Hyperbox o m
compress hyperbox = compose
  (fmap (flip compressAxis) . reverse $ [0 .. dimension hyperbox - 1])
  hyperbox

-- | Compress a hyperbox along a particular axis.
compressAxis :: Ring m => Hyperbox o m -> Int -> Hyperbox o m
compressAxis hyperbox axis =
  fixIndices axis . mapToHyperbox . compressAlong axis $ decomposeAlongAxis hyperbox axis
 where
    -- makes dn into 1
  compressAlong :: Ring m => Int -> NE.NonEmpty (MapOfHyperboxes o m) -> MapOfHyperboxes o m
  compressAlong _ ((NE.:|) h []) = h
  compressAlong axis' hs = let hs' = reverse $ NE.toList hs
                           in compose (fmap (composeAlong axis')  (init hs')) (last hs') -- checked length above
  -- decomposeAlongAxis gives [first slice,second slice,...]
  -- we reverse that into [last slice, ..., second slice, first slice]
  -- 

-- | Takes a hyperbox which is known to have size one in all dimensions into a cubical chain complex.  Does not check the size!
hyperCubeToCube
  :: Hyperbox (Set Labeling) (Labeling -> Set Labeling)
  -> CubeOf Resolution Labeling (Labeling -> Set Labeling)
hyperCubeToCube hyp = CubeOf lvl3
 where
  lvl1 = M.mapKeys (fmap intToR) $ vertices hyp
  dd   = M.keys lvl1
-- lvl2 :: Map [Resolution'] (Map o [[Resolution']])
  lvl2 = M.mapWithKey
    (\r x ->
      M.fromList $ fmap (\x' -> (x', filter (>= r) dd)) (S.toList x)
    )
    lvl1
  lvl3 =
    fmap (fmap M.fromList)
      . M.mapWithKey
          (\d xm -> fmap
            ( catMaybes
            . fmap
                (\d' -> sequence
                  (d', M.lookup (rToDelta d, epsDiff d' d) arrows'')
                )
            )
            xm
          )
      $ lvl2
  arrows'' = arrowsH hyp
  rToDelta :: Resolution -> Delta
  rToDelta = fmap why
  epsDiff :: Resolution -> Resolution -> Epsilon
  epsDiff d2 d1 = ((fromInteger . toInteger) :: Int -> Z2)
    <$> zipWith subtract (fmap why d2) (fmap why d1)

-- ** Maps of hyperboxes

-- this is like a hyperbox whose shape is guaranteed
-- | A morphism of hyperboxes is itself a hyperbox.  The last dimension of that box has length 1, so the box is the sum of @hZero@ and @hOne@.  @theMap@ consists of arrows between those two pieces.
data MapOfHyperboxes object morphism = MapOfHyperboxes {hZero :: Hyperbox object morphism,
                                                        hOne :: Hyperbox object morphism,
                                                        theMap :: Map (Delta, Epsilon) morphism}


-- axis will be the axis of the NEW stuff
-- remember that axes and insertAt both start at 0
-- assumes that all parts already 'know' which axis they belng on
mapToHyperbox :: MapOfHyperboxes o m -> Hyperbox o m
mapToHyperbox m = Hyperbox (M.union (vertices m0) (vertices m1)) arrows''
 where
  m0       = hZero m
  m1       = hOne m
  arrows'' = arrowsH m0 `M.union` arrowsH m1 `M.union` theMap m 

-- | Maps of hyperboxes are composed by stacking them along their last axis, then compressing along that axis.  Just in case, 'composeAlong' lets you choose the axis.
composeAlong
  :: Ring morphism => Int -- ^ axis along which to compose
     -> MapOfHyperboxes object morphism
     -> MapOfHyperboxes object morphism
     -> MapOfHyperboxes object morphism
composeAlong axis' m' m = MapOfHyperboxes (hZero m) (hOne m') (composeDeltaEpsilonMap axis' (theMap m') (theMap m))

-- should compose as functions!
-- | Composes maps parametrized by @(Delta, Epsilon)@.
composeDeltaEpsilonMap
  :: (Ring morphism) => Int
  -> Map (Delta, Epsilon) morphism
  -> Map (Delta, Epsilon) morphism
  -> Map (Delta, Epsilon) morphism
composeDeltaEpsilonMap axis m' m = graphMap (uncurry newMorphism) . M.keys $ m
-- it is weird that m' doesn't appear directly in the definition but that's because it appears below
 where
  newMorphism delta eps =
    rconcat . catMaybes . fmap (ePsToMap m' m axis delta) $ decompose eps
  decompose eps = if eps !! axis == ZZero
                  then [] -- if eps isn't maplike then it contributes nothing
                  else decomposeZ2Sum . replaceAt axis SumTwo . fmap singleToSum $ eps

-- | To compress a hyperbox, we think of it as a map of hyperboxes, then compose those maps.  'decomposeAlongAxis' does this for an axis.
decomposeAlongAxis
  :: Hyperbox object morphism
  -> Int -- ^ axis
  -> NonEmpty (MapOfHyperboxes object morphism)
decomposeAlongAxis h axis =
   NE.fromList . reverse $ unfoldr (splitOffLastAlongAxis axis) h
   -- this gives [first map, second map, ...]
-- * Helpers
upTo :: Delta -> [Delta]
upTo [d     ] = fmap (: []) [0 .. d]
upTo (d : ds) = [ i : ds' | ds' <- upTo ds, i <- [0 .. d] ]
upTo []       = []

upToE :: [Z2] -> [[Z2]]
upToE [ZOne      ] = [[ZZero], [ZOne]]
upToE [ZZero     ] = [[ZZero]]
upToE (ZZero : es) = [ ZZero : es' | es' <- upToE es ]
upToE (ZOne  : es) = [ i : es' | es' <- upToE es, i <- [ZZero, ZOne] ]
upToE []           = []


shape :: Hyperbox o m -> [Int]
shape = last . M.keys . vertices

dimension :: Hyperbox o m -> Int
dimension = length . shape



-- instance Ring morphism => Semigroup (MapOfHyperboxes object morphism) where
--   m' <> m = MapOfHyperboxes (hZero m) (hOne m') (composeDeltaEpsilonMap (theMap m) (theMap m'))
-- compose as functions!



fixIndices :: Int -> Hyperbox o m -> Hyperbox o m
fixIndices axis' h = h
  { vertices = M.mapKeys
                (\d -> if d !! axis' > 1 then replaceAt axis' 1 d else d)
                . vertices
                $ h
  , arrowsH  = M.mapKeys
                (\(d, e) -> if d !! axis' > 1
                    then (replaceAt axis' 1 d, e)
                    else (d, e)
                )
                . arrowsH
                $ h
  }
                                          
  
  
  

decomposeZ2Sum :: [Z2Sums] -> [(Epsilon, Epsilon)]
decomposeZ2Sum []               = []
decomposeZ2Sum [SumZero       ] = [([ZZero], [ZZero])]
decomposeZ2Sum [SumOne        ] = [([ZOne], [ZZero]), ([ZZero], [ZOne])]
decomposeZ2Sum [SumTwo        ] = [([ZOne], [ZOne])]
decomposeZ2Sum (SumZero : z2ss) = fmap (both (ZZero :)) . decomposeZ2Sum $ z2ss
decomposeZ2Sum (SumOne : z2ss) =
  concatMap (\(e1,e2) -> [(ZZero:e1, ZOne:e2), (ZOne:e1, ZZero:e2)])    $ decomposeZ2Sum z2ss
decomposeZ2Sum (SumTwo : z2ss) =  fmap (both (ZOne :)) . decomposeZ2Sum $ z2ss


data Z2Sums = SumZero | SumOne | SumTwo deriving (Eq,Ord,Show)
singleToSum :: Z2 -> Z2Sums
singleToSum ZOne  = SumOne
singleToSum ZZero = SumZero
sumZ2 :: Z2 -> Z2 -> Z2Sums
ZOne  `sumZ2` ZOne  = SumTwo
ZZero `sumZ2` x     = singleToSum x
x     `sumZ2` ZZero = singleToSum x

ePsToMap
  :: Ring morphism
  => Map (Delta, Epsilon) morphism
  -> Map (Delta, Epsilon) morphism
  -> Int
  -> Delta
  -> (Epsilon, Epsilon)
  -> Maybe morphism
ePsToMap m2 m1 axis d (e2, e1) =
  -- let sums :: [Z2Sums]
  --     sums = zipWith sumZ2 e2 e1
  --     ees = decomposeZ2Sum sums
  -- in 
    liftA2 (<<>>) ((adjustRTo axis 0 (d `addDE` e1), e2) `M.lookup` m2) ((d, e1) `M.lookup` m1)


-- allSplits :: Epsilon -> [(Epsilon, Epsilon)]
-- allSplits [] = []
-- allSplits (e:eps) = allSplits' eps (allSplits' [e] [])
-- allSplits' :: Epsilon -> [(Epsilon, Epsilon)] -> [(Epsilon, Epsilon)]
-- allSplits' [] ePs = ePs
-- allSplits' [ZZero] [] = [([ZZero],[ZZero])]
-- allSplits' [ZOne] [] = [([ZZero],[ZOne]),([ZOne],[ZZero])]
-- allSplits' (ZZero : es) ePs = allSplits' es (fmap (both (++ [ZZero])) ePs)
-- allSplits' (ZOne  : es) ePs = allSplits' es (  fmap ((++ [ZZero]) *** (++ [ZOne])) ePs
--                                               ++ fmap ((++ [ZOne]) *** (++ [ZZero])) ePs
                                            -- )

-- axes start at 0!
splitOffLastAlongAxis
  :: Int -> Hyperbox o m -> Maybe (MapOfHyperboxes o m, Hyperbox o m)
splitOffLastAlongAxis axis hyperbox =
  case lookup axis (zip [0 ..] (shape hyperbox)) of
    Nothing -> Nothing
    Just 0  -> Nothing
    Just dn
      -> let
           map' = MapOfHyperboxes
             ( adjustAllRsTo axis 0
             . getLastOnAxis axis
             . cutLastOnAxis axis
             $ hyperbox
             )
             (adjustAllRsTo axis 1 . getLastOnAxis axis $ hyperbox)
             ( M.mapKeys
                 (\(d,e) -> (adjustRTo axis 0 d, e) -- TODO: no lambda
                 ) -- 
             . M.filterWithKey
                 (\(d, e) _ -> (d !! axis) == dn - 1 && (e !! axis) == ZOne) -- mapwise arrows in the second-to-last 'column'
             . arrowsH
             $ hyperbox
             )
         in  Just (map', cutLastOnAxis axis hyperbox)


--first is axis
adjustRTo :: Int -> Int -> Delta -> Delta
adjustRTo  = replaceAt 

onRs :: (Delta -> Delta) -> Hyperbox o m -> Hyperbox o m
onRs f hyp = Hyperbox (M.mapKeys f $ vertices hyp)
                    (M.mapKeys (first f) $ arrowsH hyp)
adjustAllRsTo :: Int -> Int -> Hyperbox o m -> Hyperbox o m
adjustAllRsTo axis' n = onRs (adjustRTo axis' n)

cutLastOnAxis :: Int -> Hyperbox o m -> Hyperbox o m
cutLastOnAxis axis hyperbox = cutLastM . cutLastV $ hyperbox
 where
  dn = shape hyperbox !! axis
  cutLastM h = h { arrowsH = cutLastM' . arrowsH $ h }
  cutLastV h = h { vertices = cutLastV' . vertices $ h }
  cutLastV' m =
    compose (fmap M.delete . filter (\k -> k !! axis == dn) . M.keys $ m) m
  cutLastM' m = compose
    ( fmap M.delete
    . filter (\(d, e) -> (d !! axis == dn) || ((d `addDE` e) !! axis == dn))
    . M.keys
    $ m
    )
    m

getLastOnAxis :: Int -> Hyperbox o m -> Hyperbox o m
getLastOnAxis axis h = h
  { vertices = M.filterWithKey (\d _ -> d !! axis == dn) . vertices $ h
  , arrowsH  = M.filterWithKey (\(d, _) _ -> d !! axis == dn) . arrowsH $ h
  }
  where dn = shape h !! axis :: Int


