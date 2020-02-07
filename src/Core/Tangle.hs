{-# LANGUAGE MultiWayIf #-}
module Core.Tangle where
import           Core.PlanarDiagram
import           Core.Grid                      ( Point(..)
                                                , Edge
                                                , _y
                                                , _x
                                                , shiftYBy
                                                , shiftXBy
                                                )
import           Core.Braids             hiding ( mirror )
import Data.Semigroup ((<>), Semigroup)
import           Data.Monoid hiding ((<>))
import           Core.Util                      ( graphMap
                                                , compose
                                                )

import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as M
import           Data.Maybe
import           Data.List.Extra                ( find
                                                , nubOrd
                                                )


import           Core.Configuration
import           Prelude                 hiding ( Right
                                                , Left
                                                )
import Data.Set (Set)
import qualified Data.Set as S
type Position = Int

-- uses Khovanov's notation
-- data BasicTangle = Crossing CrossingType Position
--                  | Id 
--                  | Cap Position
--                  | Cup Position
--                  deriving (Show, Eq, Ord)


-- data Tangle = Tangle [BasicTangle] deriving (Show, Eq, Ord)

-- | Right now, tangles must be "bridge position" -- no Caps!
-- | Just draw them!


type Crossings = [CrossNode Point]
type Tangle = CrossPD
type Plats = [CrossNode Point]


-- TODO: do we need bottoms?
data OrientedMorseTangle = OMTangle { tangle :: Tangle,
                                      plats :: Plats,
                                      orientation :: Orientation} deriving (Show, Eq, Ord)


data TwoSidedLink = TSLink { diagramTSL :: CrossPD,
                             leftPlats :: Plats,
                             rightPlats :: Plats} deriving (Show, Eq, Ord)


data Orientation' = Up | Down | Left | Right deriving (Show, Eq, Ord)
type Orientation = Map Point Orientation'


data MorseTangle = MT {mtangle :: Tangle, mplats :: Plats } deriving (Show, Eq, Ord)

instance Semigroup MorseTangle where
    (<>) = dUnionMT
instance Monoid MorseTangle where
  mappend = (<>)
  mempty = MT [] []


dUnionMT :: MorseTangle -> MorseTangle -> MorseTangle
dUnionMT (MT t p) (MT t' p') = MT (t ++ shift t') (p ++ shift p')
  where shift = fmap (fmap (shiftYBy (tangleBottom t + 1)))

-- throws out inner plats...
dUnionTSL :: TwoSidedLink -> TwoSidedLink -> TwoSidedLink
dUnionTSL (TSLink d l _) (TSLink d' _ r') = TSLink d2 l2 r2
 where
  shift = fmap (fmap (shiftYBy (tangleBottom d + 1)))
  d2    = d ++ shift d'
  l2    = l
  r2    = shift r'

halfPlat :: Braid -> [Orientation'] -> OrientedMorseTangle
halfPlat b o = OMTangle
  (halfPlatClosure b)
  [ CrossJoin (Point (2 * i) 0) (Point ((2 * i) + 1) 0)
   | i <- [0 .. floor (fromIntegral (braidWidth b) / 2 - 1 :: Rational)]
   ]
  (M.fromList $ zip bottoms' o)
 where
  bottoms' =
    [ Point i (2 + 2 * (length . braidWord $ b))
     | i <- [0 .. (braidWidth b - 1)]
     ]

omtToTangle :: OrientedMorseTangle -> Tangle
omtToTangle = mtangle . omtToMt

omtToMt :: OrientedMorseTangle -> MorseTangle
omtToMt omt = MT (tangle omt) (plats omt)

onTangle :: (Tangle -> Tangle) -> MorseTangle -> MorseTangle
onTangle f (MT t p) = MT (f t) p
flipPoint :: Int -> Point -> Point
flipPoint bot (Point x y) = Point x (bot - y)

tangleBottom  :: CrossPD -> Int
tangleBottom = pdBottom
tangleWidth  :: CrossPD -> Int
tangleWidth = pdWidth

pointsOnBottom :: Tangle -> Int
pointsOnBottom t =
  length . filter ((== tangleBottom t) . _y) . nubOrd . getPointsC $ t

mirror :: Tangle -> Tangle
mirror t =
  reverseCrossing . fmap (flipPoint (tangleBottom t)) <$> t
 where
  reverseCrossing (CrossJoin a b    ) = CrossJoin a b
  reverseCrossing (Cross Pos a b c d) = Cross Neg a b c d
  reverseCrossing (Cross Neg a b c d) = Cross Pos a b c d

rotateT :: Tangle -> Tangle
rotateT tang =
  let bot   = tangleBottom tang
      width = tangleWidth tang
      rotatePoint (Point x y) = Point (width - x) (bot - y)
      fixCrossing (Cross s a b c d) = Cross s d c b a
  in  fmap
        (\n ->
          if isCross n then fixCrossing . fmap rotatePoint $ n else fmap rotatePoint n
        )
        tang

mirrorMorse :: MorseTangle -> MorseTangle
mirrorMorse (MT t p) = MT
  (fmap (\n -> if isCross n then fixCrossing n else n) . mirror $ t)
  (fmap (flipCrossNode . fmap (flipPoint (tangleBottom t))) p)
 where
  fixCrossing (Cross s a b c d) = Cross s c d a b
  flipCrossNode (Cross s a b c d) = Cross s c a d b
  flipCrossNode (CrossJoin a b  ) = CrossJoin b a

rotateOMT :: OrientedMorseTangle -> OrientedMorseTangle
rotateOMT (OMTangle t p o) = OMTangle t' p' o'
 where
  t'    = rotateT t
  bot   = tangleBottom t
  width = tangleWidth t
  rotatePoint (Point x y) = Point (width - x) (bot - y)
  p' = fmap (fmap rotatePoint) p
  o' = M.mapKeys rotatePoint . fmap flipO $ o

-- -- really not clear to me that bottoms is used
mirrorOMT :: OrientedMorseTangle -> OrientedMorseTangle
mirrorOMT (OMTangle t p o) = OMTangle t' p' o'
 where
  MT t' p' = mirrorMorse (MT t p)
  o'       = mirrorO o
  mirrorO :: Orientation -> Orientation
  mirrorO = fmap flipO . M.mapKeys (flipPoint (tangleBottom t))

flipO :: Orientation' -> Orientation'
flipO Up   = Down
flipO Down = Up
flipO Left = Right
flipO Right = Left



glueMorseTangle :: MorseTangle -> MorseTangle -> TwoSidedLink
glueMorseTangle (MT t1 p1) (MT t2 p2) = TSLink (t1 `glueTangle` t2) p1 newP2
 where
  newP2 =
    fmap (fmap (shiftYBy (tangleBottom t1) . flipPoint (tangleBottom t2))) p2

glueTangle :: Tangle -> Tangle -> Tangle
glueTangle t t' = t <> fmap (fmap (shiftYBy (tangleBottom t))) (mirror t')

stringToOrientedMorseTangle :: String -> OrientedMorseTangle
stringToOrientedMorseTangle t =
  stringToOrientedMorseTangle' (fmap (fmap (: [])) . lines $ t)

stringToOrientedMorseTangle' :: [[String]] -> OrientedMorseTangle
stringToOrientedMorseTangle' tss =
  let (_, es, cs, pls, os) = stringToTangleFirst tss
      t                     = fmap (uncurry CrossJoin) es ++ cs
      plats'                 = fmap (uncurry CrossJoin) pls
  in  OMTangle t plats' os

stringToPD :: String -> CrossPD
stringToPD =
  (\(_, es, cs, pls, _) -> (uncurry CrossJoin <$> (es ++ pls)) ++ cs)
    . stringToTangleFirst
    . fmap (fmap (: []))
    . lines

stringToTangleFirst
  :: [[String]] -> ([Point], [Edge], Crossings, [Edge], Orientation)
stringToTangleFirst = stringToGrid' 0
 where
  stringToGrid' j (row : rows) =
    parseRowTangle j row <> stringToGrid' (j + 1) rows
  stringToGrid' _ [] = mempty
  parseRowTangle
    :: Int -> [String] -> ([Point], [Edge], Crossings, [Edge], Orientation)
  parseRowTangle y xs = go y 0 xs ([], [], [], [], M.empty)
  go y i (x : xs) (ps, es, cs, plats', o) = case even y of
    True
      | x == "." -> go y (i + 1) xs (currentPoint i y : ps, es, cs, plats', o)
      | x == "V" -> go
        y
        (i + 1)
        xs
        ( currentPoint i y : ps
        , es
        , cs
        , plats'
        , o <> M.singleton (currentPoint i y) Down
        )
      | x == "^" -> go
        y
        (i + 1)
        xs
        ( currentPoint i y : ps
        , es
        , cs
        , plats'
        , o <> M.singleton (currentPoint i y) Up
        )
      | x == " " -> go y (i + 1) xs (ps, es, cs, plats', o)
      | x == "-" -> go
        y
        (i + 1)
        xs
        (ps, (currentPoint i y, currentPoint (i + 2) y) : es, cs, plats', o)
      | x == "p" -> go
        y
        (i + 1)
        xs
        ( ps
        , (currentPoint i y, currentPoint (i + 2) y) : es
        , cs
        , (currentPoint i y, currentPoint (i + 2) y) : plats'
        , o
        )
      | otherwise -> error
        ("not a valid grid (even): " ++ x ++ "at position " ++ show (i, y))

    False -> case even i of
      True
        | x == " " -> go y (i + 1) xs (ps, es, cs, plats', o)
        | x == "|" -> go
          y
          (i + 1)
          xs
          (ps, (currentPoint i y, currentPoint i (y + 1)) : es, cs, plats', o)
        | otherwise -> error
          ("not a valid grid (odd, even): " ++ x ++ "at position " ++ show
            (i, y)
          )

      False
        | x == " " -> go y (i + 1) xs (ps, es, cs, plats', o)
        | x == "X" -> go
          y
          (i + 1)
          xs
          ( ps
          , es
          , Cross Neg
                  (currentPoint i y)
                  (currentPoint (i + 2) y)
                  (currentPoint i (y + 2))
                  (currentPoint (i + 2) (y + 2))
            : cs
          , plats'
          , o
          )
        | x == "+" -> go
          y
          (i + 1)
          xs
          ( ps
          , es
          , Cross Pos
                  (currentPoint i y)
                  (currentPoint (i + 2) y)
                  (currentPoint i (y + 2))
                  (currentPoint (i + 2) (y + 2))
            : cs
          , plats'
          , o
          )
        | otherwise -> error
          ("not a valid grid (odd): " ++ x ++ "at position " ++ show (i, y))
  go _ _ [] stuff = stuff
  currentPoint i y = Point (i `div` 2) (y `div` 2)

data CrossingOrientation = Positive | Negative deriving (Show, Eq, Ord)
data StrandOrientation = In | Out deriving (Show, Eq, Ord)

orientedCrossings
  :: OrientedMorseTangle -> Map (CrossNode Point) CrossingOrientation
orientedCrossings (OMTangle t _ o) =
  let crosses = filter isCross t in graphMap (orientCrossing t o) crosses

orientCrossing
  :: Tangle -> Orientation -> CrossNode Point -> CrossingOrientation
orientCrossing ns o (Cross sense a b _ _) =
  let o'  = orientPoint a ns o
      o'' = orientPoint b ns o
  in  getO sense (oToO o') (oToO o'')

-- assuming from the bottom
oToO :: Orientation' -> StrandOrientation
oToO Up   = In
oToO Down = Out

findConnect :: Point -> Set (CrossNode Point) -> Maybe (CrossNode Point)
findConnect p = find (connects p)
 where
  connects p' (CrossJoin a b  ) = p' == a || p' == b
  connects p' (Cross _ a b c d) = p' == a || p' == b || p == c || p == d

-- run out from the point, then check what the orientation is
orientPoint :: Point -> [CrossNode Point] -> Orientation -> Orientation'
orientPoint p ns o =
  let pp = orientFrom p ns
  in  if not (isCross pp)
        then
          let CrossJoin p' q' = pp
          in  if p' `elem` M.keys o then o M.! p' else o M.! q'
        else
          let Cross _ p' q' r' s' = pp
          in  maybe (error "bad orientPoint")
                    (o M.!)
                    (find (`elem` M.keys o) [p', q', r', s'])




-- TODO: clean-up!
-- TODO: split into its own module
orientFrom :: Point -> [CrossNode Point] -> CrossNode Point
orientFrom p' ns = orientFrom' p' (S.fromList ns) S.empty S.empty Nothing
 where
  ns' = S.fromList ns
  orientFrom'
    :: Point
    -> Set (CrossNode Point)
    -> Set (CrossNode Point)
    -> Set (CrossNode Point)
    -> Maybe (CrossNode Point)
    -> CrossNode Point
  orientFrom' p nodes alreadySeenNodes alreadySeenCrosses justSeen =
    case
        findConnect
          p
          (nodes `S.difference` (alreadySeenNodes `S.union` maybeToSetWith isCross justSeen))
      of
        Just c@(CrossJoin a b) -> if
          | p == a -> orientFrom' b
                                  nodes
                                  (S.insert c alreadySeenNodes)
                                  alreadySeenCrosses
                                  (Just c)
          | p == b -> orientFrom' a
                                  nodes
                                  (S.insert c alreadySeenNodes)
                                  alreadySeenCrosses
                                  (Just c)
        Just c@(Cross _ e f g h) -> if c `elem` alreadySeenCrosses
          then if
            | p == e -> orientFrom' g
                                    ns'
                                    (S.insert c alreadySeenNodes)
                                    alreadySeenCrosses
                                    (Just c)
            | p == g -> orientFrom' e
                                    ns'
                                    (S.insert c alreadySeenNodes)
                                    alreadySeenCrosses
                                    (Just c)
            | p == f -> orientFrom' h
                                    ns'
                                    (S.insert c alreadySeenNodes)
                                    alreadySeenCrosses
                                    (Just c)
            | p == h -> orientFrom' f
                                    ns'
                                    (S.insert c alreadySeenNodes)
                                    alreadySeenCrosses
                                    (Just c)
          else if
            | p == e -> orientFrom' g
                                    ns'
                                    alreadySeenNodes
                                    (S.insert c alreadySeenCrosses)
                                    (Just c)
            | p == g -> orientFrom' e
                                    ns'
                                    alreadySeenNodes
                                    (S.insert c alreadySeenCrosses)
                                    (Just c)
            | p == f -> orientFrom' h
                                    ns'
                                    alreadySeenNodes
                                    (S.insert c alreadySeenCrosses)
                                    (Just c)
            | p == h -> orientFrom' f
                                    ns'
                                    alreadySeenNodes
                                    (S.insert c alreadySeenCrosses)
                                    (Just c)
        Nothing -> fromJust justSeen

numCT :: CrossingType -> Int
numCT Pos = 1
numCT Neg = -1

-- from bottom
numSO :: StrandOrientation -> Int
numSO Out = -1
numSO In  = 1

coNum :: Int -> CrossingOrientation
coNum 1    = Positive
coNum (-1) = Negative

getO
  :: CrossingType
  -> StrandOrientation
  -> StrandOrientation
  -> CrossingOrientation
getO a b c = coNum (numCT a * numSO b * numSO c)


orientedResolution :: OrientedMorseTangle -> OrientedMorseTangle
orientedResolution (OMTangle t p o) = OMTangle t' p o
 where
  t' = concatMap orientedRes' t
  orientedRes' :: CrossNode Point -> [CrossNode Point]
  orientedRes' c =
    if isCross c then orientedResolution' (orientCrossing t o c) c else [c]
  orientedResolution'
    :: CrossingOrientation -> CrossNode Point -> [CrossNode Point]
  orientedResolution' _ (CrossJoin q q') = [CrossJoin q q']
  orientedResolution' Positive (Cross _ x y z w) =
    [CrossJoin x y, CrossJoin z w]
  orientedResolution' Negative (Cross _ x y z w) =
    [CrossJoin x z, CrossJoin y w]

orientationToNodes :: Orientation -> [Node Point]
orientationToNodes =
  fmap
      (\(p, o) ->
        if o == Up then Join p (shiftYBy 1 p) else Join p (shiftYBy (-1) p)
      )
    . M.toList

applyOrientation :: (Point, Orientation') -> (Point, Point)
applyOrientation (p, Up   ) = (p, shiftYBy (-1) p)
applyOrientation (p, Down ) = (shiftYBy (-1) p, p)
applyOrientation (p, Right) = (p, shiftXBy 1 p)
applyOrientation (p, Left ) = (p, shiftXBy (-1) p)
-- TODO: work out this inconsistency

-- last two are incoming
data MarkedCrossing = MarkedCrossing CrossingType Point Point Point Point (Maybe Point) (Maybe Point) deriving (Eq,Ord,Show)

removeMarkings :: MarkedCrossing -> CrossNode Point
removeMarkings (MarkedCrossing s a b c d _ _) = Cross s a b c d

unmarked :: CrossNode Point -> MarkedCrossing
unmarked (Cross s a b c d) = MarkedCrossing s a b c d Nothing Nothing
mark :: Point -> MarkedCrossing -> MarkedCrossing
mark p (MarkedCrossing s a b c d Nothing Nothing) =
  MarkedCrossing s a b c d (Just p) Nothing
mark p (MarkedCrossing s a b c d e Nothing) =
  MarkedCrossing s a b c d e (Just p)
mark p m@(MarkedCrossing _ _ _ _ _ (Just _) (Just _)) =
  error $ "tried to mark a crossing three times: " ++ show p ++ ", " ++ show m

inMarked
  :: (Functor t, Foldable t) => CrossNode Point -> t MarkedCrossing -> Bool
inMarked c mcs = c `elem` fmap removeMarkings mcs
inMarkedSet :: CrossNode Point -> Set MarkedCrossing -> Bool
inMarkedSet c mcs = c `S.member` S.map removeMarkings mcs

addMarkTo :: Point -> CrossNode Point -> Set MarkedCrossing -> Set MarkedCrossing
addMarkTo p c =
  S.map (\mc -> if removeMarkings mc == c then mark p mc else mc)

resolveMarkedCrossing :: MarkedCrossing -> [CrossNode Point]
resolveMarkedCrossing (MarkedCrossing _ a b c d (Just e) (Just f)) = if
  | (a, b) == (e, f) || (a, b) == (f, e) -> [CrossJoin a c, CrossJoin b d]
  | (c, d) == (e, f) || (c, d) == (f, e) -> [CrossJoin c a, CrossJoin d b]
  | (a, c) == (e, f) || (a, c) == (f, e) -> [CrossJoin a b, CrossJoin c d]
  | (b, d) == (e, f) || (b, d) == (f, e) -> [CrossJoin b a, CrossJoin d c]
resolveMarkedCrossing m = error $ show m

orientConsistently :: PD -> Orientation -> PD
orientConsistently pd o' =
  let fixedPoints :: [(Point, Point)]
      fixedPoints = fmap applyOrientation . M.toList $ o'
  in  consistentlyOrient pd fixedPoints

verticalStrands :: Int -> Int -> [CrossNode Point]
verticalStrands width level =
  [ CrossJoin (Point i level) (Point i (level + 1)) | i <- [0 .. width - 1] ]
halfPlatClosure :: Braid -> CrossPD
halfPlatClosure braid =
  platTop width
    ++ concat
         [ crossingToNodes c width (2 * d + 1)
             ++ verticalStrands width (2 * (d + 1))
         | (c, d) <- zip word [0 ..]
         ]
    ++ verticalStrands width bot
 where
  bot =
    tangleBottom (platTop width)
      + tangleBottom
          (concat
            [ crossingToNodes c width d ++ verticalStrands width (2 * (d + 1))
            | (c, d) <- zip word [1 ..]
            ]
          )
      - 3
  width = braidWidth braid
  word  = braidWord braid

platClosure :: Braid -> CrossPD
platClosure braid =
  platTop width
    ++ concat [ crossingToNodes2 c width d | (c, d) <- zip word [1 ..] ] -- used to have shiftNodeYBy 1 <$>
    ++ platBottom width (length word)
 where
  width = braidWidth braid
  word  = braidWord braid

nodeToOrientation :: CrossNode Point -> Orientation'
nodeToOrientation (CrossJoin a b) 
  | _x a == _x b = if _y a > _y b then Up else Down
  | _x a > _x b = Left
  | otherwise = Right
nodeToOrientation c = error $ "tried to nodeToOrientation a crossing: " ++ show c 

crossOrientNew :: CrossPD -> Orientation -> PD
crossOrientNew cppd o =
  flip orientConsistently
      (         M.filter (Up ==) o
      `M.union` ( M.mapKeys (\(CrossJoin a _) -> a)
                . graphMap nodeToOrientation
                $ resolvedCrossings
                )
      )
    . unsafeCrossToFlat
    . (resolvedCrossings ++)
    . filter (not . isCross)
    $ cppd
 where
  resolvedCrossings =
    concatMap resolveMarkedCrossing
      . orientWithCrossings (M.filter (Up ==) o)
      $ cppd
  orientWithCrossings :: Orientation -> CrossPD ->  [MarkedCrossing]
  orientWithCrossings o' cpd =  S.toList  . snd . ($ (cpd, S.empty)) . compose . fmap orientTheCrossings' . M.toList $ o'

orientTheCrossings':: (Point, Orientation')
  -> (CrossPD, Set MarkedCrossing)
  -> (CrossPD, Set MarkedCrossing)
orientTheCrossings' (p, o') (cpd, marks) = orientFrom'' (snd $ applyOrientation (p, o')) (S.fromList cpd, marks) S.empty (Just (uncurry CrossJoin (applyOrientation (p, o'))))
-- TODO: work out if we're actually using justSeen, etc.
maybeToSetWith :: (a -> Bool) -> Maybe a -> Set a
maybeToSetWith _ Nothing = S.empty
maybeToSetWith f (Just a) = if f a then S.singleton a else S.empty

-- TODO: embarssing that there are two orientFrom's!!

orientFrom'' :: Point
  -> (Set (CrossNode Point), Set MarkedCrossing)
  -> Set (CrossNode Point)
  -> Maybe (CrossNode Point)
  -> (CrossPD, Set MarkedCrossing)
orientFrom'' p (nodes, marked) alreadySeenNodes justSeen = case
  findConnect p (nodes  `S.difference` (alreadySeenNodes `S.union` maybeToSetWith (const True) justSeen))
    of
    Just c@(CrossJoin a b) -> if
        | p == a -> orientFrom'' b
                                (S.delete c nodes, marked)
                                (S.insert c alreadySeenNodes)
                                (Just c)
        | p == b -> orientFrom'' a
                                (S.delete c nodes, marked)
                                (S.insert c alreadySeenNodes)
                                (Just c)
    Just c@(Cross _ e f g h) -> if c `inMarkedSet` marked
        then if
        | p == e -> orientFrom'' h
                                (S.delete c nodes, addMarkTo p c marked)
                                alreadySeenNodes
                                (Just c)
        | p == h -> orientFrom'' e
                                (S.delete c nodes, addMarkTo p c marked)
                                alreadySeenNodes
                                (Just c)
        | p == f -> orientFrom'' g
                                (S.delete c nodes, addMarkTo p c marked)
                                alreadySeenNodes
                                (Just c)
        | p == g -> orientFrom'' f
                                (S.delete c nodes, addMarkTo p c marked)
                                alreadySeenNodes
                                (Just c)
        else if
        | p == e -> orientFrom'' h
                                (nodes, S.insert (mark p . unmarked $ c) marked)
                                alreadySeenNodes
                                (Just c)
        | p == g -> orientFrom''
            f
            (nodes, S.insert (mark p . unmarked $ c) marked)
            alreadySeenNodes
            (Just c)
        | p == f -> orientFrom''
            g
            (nodes, S.insert (mark p . unmarked $ c) marked)
            alreadySeenNodes
            (Just c)
        | p == h -> orientFrom''
            e
            (nodes, S.insert (mark p . unmarked $ c) marked)
            alreadySeenNodes
            (Just c)
    Nothing -> (S.toList $ nodes `S.union` alreadySeenNodes, marked)

