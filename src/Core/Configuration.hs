module Core.Configuration where
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , isJust
                                                )
import qualified Data.Map                      as M
import           Data.List.Extra                ( find
                                                , (\\)
                                                , nubOrd
                                                )
-- import qualified Data.Graph                    as G
import           GHC.Generics                   ( Generic )
import           Algebra.Graph.AdjacencyMap
                                         hiding ( compose )
import qualified Algebra.Graph.NonEmpty.AdjacencyMap
                                               as NEAM
import           Algebra.Graph.AdjacencyMap.Algorithm
import           Core.Grid
import           Core.PlanarDiagram      
import           Core.Util


-- Basics

-- should always be oriented!
-- could make components and activeGraph lazy if necessary
data Configuration = Configuration {diagram :: PD,
                                    decos :: Set Decoration,
                                    components :: Set Component,
                                    aGraph :: AdjacencyMap PD
                                   } deriving (Eq,Show, Ord)


drdToConfig :: DecoratedResolvedDiagram -> Configuration
drdToConfig (DRDiagram pd decos') =
  orientConfig $ Configuration pd (S.fromList decos') S.empty empty

-- YOU SHOULD NOT BUILD CONFIGURATIONS LIKE THIS BECAUSE ALGORITHMS ASSUME CONFIGURATIONS ARE ORIENTED

-- Basic operations

reverseConfig :: Configuration -> Configuration
reverseConfig (Configuration diag decos' c g) =
  Configuration diag (S.map reverseDeco decos') c g

reverseDeco :: Decoration' a -> Decoration' a
reverseDeco (Decoration x y) = Decoration y x

buildConfig :: Set PD -> Set Decoration -> Configuration
buildConfig comps decs =
  orientConfig $ Configuration (overlays . S.toList $ comps) decs S.empty empty

-- rebuildConfig :: Configuration -> Configuration
-- rebuildConfig conf = buildConfig (S.singleton $ diagram conf) (decos conf)

platDecorations :: CrossNode Point -> CrossNode Point -> Decoration
platDecorations (CrossJoin p p') (CrossJoin q q') =
  Decoration (Arc p p') (Arc q q')
-- Queries
findEdge :: Configuration -> Point -> Point -> Maybe Component
findEdge c p p' =
  find (\x -> p `S.member` x && p' `S.member` x) . S.toList $ components c

findNodePD :: PD -> Point -> Point -> Maybe Component
findNodePD pd p p' =
  find (\x -> p `S.member` x && p' `S.member` x) . pdComponents $ pd

findPoint :: Configuration -> Point -> Maybe Component
findPoint c p = find (\x -> p `S.member` x) . S.toList $ components c

findPointPD :: PD -> Point -> PD
findPointPD pd p = searchPD (== p) pd

disconnected :: Configuration -> Bool
disconnected conf =
  length (vertexList . scc . symmetricClosure . aGraph $ conf) /= 1

intervals :: PD -> Decoration -> (PD, PD)
intervals pd (Decoration (Arc p q) (Arc p' q')) =
  getPQ . removeEdge p' q' . removeEdge p q $ pd
 where
  getPQ pd' = if findPointPD pd' p == findPointPD pd' p'
    then (findPointPD pd' p, findPointPD pd' q')
    else (findPointPD pd' p, findPointPD pd' p')

data DecDirection = Horizontal | Vertical | NeitherBad deriving (Eq, Ord, Show)

-- only designed for rectilinear arrows!  defaults to Horizontal
getDecDirection :: Decoration -> DecDirection
getDecDirection (Decoration (Arc p p') _) =
  if _x p /= _x p' then Vertical else Horizontal

decoIntersectsComps :: Decoration -> PD -> [Component]
decoIntersectsComps dec pd = relevant
 where
  comps    = pdComponents pd
  relevant = filter (intersects dec) comps
  -- is r between p and q?
  intersects :: Decoration -> Component -> Bool
  intersects (Decoration arc1 arc2) comp = any (isBetween arc1 arc2) (S.toList comp)
  -- isBetweenN (n, n') (Join r1 r2) = isBetween n n' r1 || isBetween n n' r2
  isBetween :: Arc -> Arc -> Point -> Bool
  isBetween n@(Arc p1 p2) n'@(Arc q1 _) r =
    case getDecDirection (Decoration n n') of
      Horizontal ->
        ((_y r == _y p1) || (_y r == _y p2)) && between (_x p1) (_x r) (_x q1)
      Vertical ->
        ((_x r == _x p1) || (_x r == _x p2)) && between (_y p1) (_y r) (_y q1)
      NeitherBad -> error "why are you using this bad decoration"
  between a b c = ((a >= b) && (b >= c)) || ((c >= b) && (b >= a))

-- each PD is a component!
-- just assume that graph is oriented to start with
activeGraph :: PD -> Set Decoration -> AdjacencyMap PD
activeGraph pd decos' =
  let comps = S.map fromNonEmptyAM . vertexSet . scc $ pd
  in  overlays . S.toList . S.map (decoToGraph comps) $ decos'
 where
  findArcInComponent :: Arc -> AdjacencyMap Point -> Bool
  findArcInComponent (Arc x _) = hasVertex x
  decoToGraph
    :: Set (AdjacencyMap Point)
    -> Decoration
    -> AdjacencyMap (AdjacencyMap Point)
  -- TODO: make this type less silly
  decoToGraph graphs (Decoration arc arc') =
    let toG   = find (findArcInComponent arc) graphs
        fromG = find (findArcInComponent arc') graphs
    in  if isJust fromG && isJust toG
          then fromNonEmptyAM $ NEAM.connect (NEAM.vertex $ fromJust fromG)
                                             (NEAM.vertex $ fromJust toG)
          else empty

activePart :: Configuration -> AdjacencyMap PD
activePart conf = induce
  (\x -> adjacencyMap (symmetricClosure (aGraph conf)) M.! x /= S.empty)
  (aGraph conf)

countActiveCircles :: Configuration -> Int
countActiveCircles = S.size . S.filter ((> 0) . edgeCount) . vertexSet . aGraph

activeCircles :: Configuration -> PD
activeCircles =
  overlays . S.toList . S.filter ((> 0) . edgeCount) . vertexSet . aGraph

activeConf :: Configuration -> Configuration
activeConf conf = Configuration
  (activeCircles conf)
  (decos conf)
  (S.fromList . pdComponents . activeCircles $ conf)
  (aGraph conf)

-- Properties of components

indegree :: Component -> Configuration -> Int
indegree comp =
  length . filter (all (`S.member` comp) . arcPoints . to) . S.toList . decos

outdegree :: Component -> Configuration -> Int
outdegree comp =
  length . filter (all (`S.member` comp) . arcPoints . from) . S.toList . decos

indegreePD :: PD -> Configuration -> Int
indegreePD pd = length . filter (\x -> arcInPD (to x) pd) . S.toList . decos

outdegreePD :: PD -> Configuration -> Int
outdegreePD pd = length . filter (\x -> arcInPD (to x) pd) . S.toList . decos

---- Attaching handles
decorationAction :: Decoration -> Configuration -> Maybe Configuration
decorationAction de config =
  case longOneHandleAttachment de (diagram config) of
    Nothing -> Nothing -- if you can't find de in config, stop
    Just _  -> Just config' -- if you can, return config'
 where
  newDiagram = fromJust . longOneHandleAttachment de $ diagram config
  config'    = buildConfig (S.singleton newDiagram) decos'
  decos'     = S.delete de (decos config)

longOneHandleAttachment :: Decoration -> PD -> Maybe PD
longOneHandleAttachment (Decoration (Arc a b) (Arc c d)) pd =
  if all (`S.member` vertexSet pd) [a, b, c, d]
    then
      Just
      $ orientPD . overlay (aToC `overlay` bToD)
      . removeEdge b a
      . removeEdge a b
      . removeEdge d c
      . removeEdge c d
      $ pd -- if you can, return config'
    else Nothing
 where
  (aToC, bToD) = if _x a == _x b -- vertical edges
    then
      ( path [ Point i (_y a) | i <- [min (_x a) (_x c) .. max (_x a) (_x c)] ]
      , path [ Point i (_y b) | i <- [min (_x b) (_x d) .. max (_x b) (_x d)] ]
      )
    else -- horizontal edges
      ( path [ Point (_x a) j | j <- [min (_y a) (_y c) .. max (_y a) (_y c)] ]
      , path [ Point (_x b) j | j <- [min (_y b) (_y d) .. max (_y b) (_y d)] ]
      )

applyAllDecorations :: Configuration -> Maybe Configuration
applyAllDecorations conf =
  kleisliCompose (fmap decorationAction (S.toList . decos $ conf)) conf

-- note that these do not check if a deco goes to a deleted comp!

deleteCompPD :: PD -> Component -> PD
deleteCompPD pd comp = compose (fmap removeVertex (S.toList comp)) pd

deleteCompConf :: Configuration -> Component -> Configuration
deleteCompConf conf comp = Configuration
  (compose (fmap removeVertex (S.toList comp)) (diagram conf))
  (decos conf)
  (S.delete comp (components conf))
  (induce (\c -> not $ null (vertexSet c `S.intersection` comp)) $ aGraph conf)

deleteComps :: Configuration -> [Component] -> Configuration
deleteComps conf comps' = Configuration
  (compose (fmap removeVertex (concatMap S.toList comps')) (diagram conf))
  (decos conf)
  (S.filter (`notElem` comps') $ components conf)
  ( induce (\c -> not $ null (vertexSet c `S.intersection` S.unions comps'))
  $ aGraph conf
  )



-- Internal

data SzabosConfigs =   A Int
                     | B Int
                     | C Int Int
                     | D Int Int
                     | E Int Int
                     | Disconnected
                     | KhovanovConfig
                     | PassiveConfig
                     | NoneOfTheAbove
                     deriving (Show, Eq,Ord, Generic)



connectedComponents :: Configuration -> [AdjacencyMap PD]
connectedComponents =
  fmap fromNonEmptyAM . vertexList . scc . symmetricClosure . aGraph

connectedActiveConfigs :: Configuration -> [Configuration]
connectedActiveConfigs conf = fmap buildAConfig (connectedComponents conf)
 where
  buildAConfig :: AdjacencyMap PD -> Configuration
  buildAConfig comps
    = let
        thePoints = concatMap getPoints . vertexList $ comps
        pd        = overlays . vertexList $ comps
        decos' =
          S.filter (\(Decoration a _) -> head (arcPoints a) `elem` thePoints)
            $ decos conf
      in
        Configuration pd
                      decos'
                      (S.fromList . pdComponents $ pd)
                      (activeGraph pd decos')
  -- fmap (uncurry buildConfig
  --   . first S.singleton
  --   . graph
  --         (\pd -> S.filter (any (`elem` getPoints pd) . decPoints)
  --                          (decos conf)
  --         )
  --   . overlays . vertexList)
  --   . connectedComponents
  --   $ conf

data CrossDeco' a  = CrossDeco' (CrossNode a) (CrossNode a)  deriving (Eq, Ord, Show, Functor)
type CrossDeco = CrossDeco' Point
data R2Type = Braidlike | AntiBraidlike deriving (Eq, Ord, Show)
data CrossDecoOrientation = CDUp | CDDown | CDLeft | CDRight | BadCrossDeco deriving (Eq, Ord, Show)
-- TODO: use getPoints more
determineR2TypeConfig :: Configuration -> CrossDeco -> R2Type
determineR2TypeConfig conf = determineR2Type (diagram conf)

determineR2Type :: PD -> CrossDeco -> R2Type
determineR2Type pd cd@(CrossDeco' c1 c2) =
  let o    = determineOrientation cd
      blr1 = unsafeCrossToFlat . braidlikeResolve o $ c1 :: PD
      blr2 = unsafeCrossToFlat . braidlikeResolve o $ c2 :: PD
  in  if (blr1 + blr2) `isSubgraphOf` symmetricClosure pd then Braidlike else AntiBraidlike

braidlikeResolve :: CrossDecoOrientation -> CrossNode Point -> [CrossNode Point]
braidlikeResolve _            (CrossJoin a b  ) = [CrossJoin a b]
braidlikeResolve CDUp         (Cross _ a b c d) = [CrossJoin a c, CrossJoin b d]
braidlikeResolve CDDown       c                 = braidlikeResolve CDUp c
braidlikeResolve CDLeft       (Cross _ a b c d) = [CrossJoin a b, CrossJoin c d]
braidlikeResolve CDRight      c                 = braidlikeResolve CDLeft c
braidlikeResolve BadCrossDeco c = error $ "BadCrossDeco: " ++ show c

antiBraidlikeResolve
  :: CrossDecoOrientation -> CrossNode Point -> [CrossNode Point]
antiBraidlikeResolve _ (CrossJoin a b) = [CrossJoin a b]
antiBraidlikeResolve CDUp (Cross _ a b c d) = [CrossJoin a b, CrossJoin c d]
antiBraidlikeResolve CDDown c = antiBraidlikeResolve CDUp c
antiBraidlikeResolve CDLeft (Cross _ a b c d) = [CrossJoin a c, CrossJoin b d]
antiBraidlikeResolve CDRight c = antiBraidlikeResolve CDLeft c
antiBraidlikeResolve BadCrossDeco c = error $ "BadCrossDeco: " ++ show c


determineOrientation :: CrossDeco -> CrossDecoOrientation
determineOrientation (CrossDeco' c1@(Cross _ a b c d) c2@(Cross _ e f g h)) =
  let averageX1 =
        sum . fmap (\p -> fromIntegral (_x p) / 4) $ [a, b, c, d] :: Rational
      averageX2 =
        sum . fmap (\p -> fromIntegral (_x p) / 4) $ [e, f, g, h] :: Rational
      averageY1 =
        sum . fmap (\p -> fromIntegral (_y p) / 4) $ [a, b, c, d] :: Rational
      averageY2 =
        sum . fmap (\p -> fromIntegral (_y p) / 4) $ [e, f, g, h] :: Rational
  in  if
        | averageX1 == averageX2 && averageY1 > averageY2 -> CDUp
        | averageX1 == averageX2 && averageY1 < averageY2 -> CDDown
        | averageY1 == averageY2 && averageX1 > averageX2 -> CDLeft
        | averageY2 == averageY2 && averageX1 < averageX2 -> CDRight
        | otherwise -> error $ "Bad: " ++ show c1 ++ " " ++ show c2
determineOrientation _ =
  error
    "Tried to determineOrientation of a CrossDeco attached to at least one CrossJoin."


attachCrossDec :: CrossDeco -> PD -> Maybe Decoration
attachCrossDec dec' pd = case determineR2Type pd dec' of
  Braidlike     -> Nothing
  AntiBraidlike -> Just . fst $ uncross dec' (determineOrientation dec')

-- want FARTHEST points attached
-- returns (uncrossed decroration, (outer arc, outer arc))
uncross :: CrossDeco -> CrossDecoOrientation -> (Decoration,(Arc,Arc))
uncross (CrossDeco' (Cross _ a b c d) (Cross _ a' b' c' d')) CDRight =
  (Decoration (Arc a c) (Arc b' d'), (Arc b d, Arc a' c')) 
  -- (Decoration (Arc b' d') (Arc a c), (Arc a' c', Arc b d))
uncross (CrossDeco' (Cross _ a b c d) (Cross _ a' b' c' d')) CDLeft =
  (Decoration (Arc b' d') (Arc a c), (Arc a' c', Arc b d))
  -- (Decoration (Arc a' c') (Arc b d), (Arc b' d', Arc a c))
uncross (CrossDeco' (Cross _ a b c d) (Cross _ a' b' c' d')) CDDown =
  (Decoration (Arc a b) (Arc c' d'), (Arc c d, Arc a' b'))
  -- (Decoration (Arc c d) (Arc a' b'), (Arc a b, Arc c' d'))
uncross (CrossDeco' (Cross _ a b c d) (Cross _ a' b' c' d')) CDUp =
  (Decoration (Arc c' d') (Arc a b), (Arc a' b', Arc c d))
  -- (Decoration (Arc a b) (Arc c' d'), (Arc c d, Arc a' b'))
uncross c BadCrossDeco = error $ show "BadCrossDeco: " ++ show c
uncross _ _            = error
  "Tried to uncross a CrossDeco between at least one CrossJoin -- not good!"


flipCrossDeco :: CrossDeco' a -> CrossDeco' a
flipCrossDeco (CrossDeco' c1 c2) = CrossDeco' c2 c1


-- Orientations

-- Orientations
-- currently assumes that every component has a fixed point
-- and that orientation is consistent.
consistentlyOrient :: PD -> [(Point, Point)] -> PD
consistentlyOrient pd fixedPoints = overlays $ fmap (orientFrom pd) fixedPoints
 where
  orientFrom :: PD -> (Point, Point) -> PD
  orientFrom pd' (p, q) =
    circuit
      . dfs [q]
      . removeEdge q p
      . removeEdge p q
      . searchPD (== p)
      $ symmetricClosure pd'
-- ensures that every component is oriented
orientConfig :: Configuration -> Configuration
orientConfig (Configuration pd decs _ _) =
  let newPD    = orientPD pd
      newDecos = S.map (fixD (orientPD pd)) decs
  in  Configuration newPD
                    newDecos
                    (S.fromList $ pdComponents newPD)
                    (activeGraph newPD newDecos)

orientPD :: PD -> PD
orientPD pd = overlays . fmap circuit $ compsAsLists pd'
 where
  pd' = vertexList pd
  compsAsLists (p : ps) =
    let aComp = dfs [p] (symmetricClosure pd)
    in  aComp : compsAsLists (ps \\ aComp)
  compsAsLists [] = []

-- only run on oriented component
getOrientation :: PD -> Point -> Orientation
getOrientation pd p =
  let theComp     = searchPD (== p) pd
      theFarthest = farthest . vertexList $ theComp
  in  checkDirection . fromJust . findNextTo theFarthest . edgeList $ theComp
 where
  findNextTo :: Point -> [(Point, Point)] -> Maybe (Point, Point, Point)
  findNextTo q edges' =
    let p' = fmap fst . find ((== q) . snd) $ edges'
        r = fmap snd . find ((== q) . fst) $ edges'
        ok (Just a , Just b , Just c ) = Just (a, b, c)
        ok (Nothing, _      , _      ) = Nothing
        ok (_      , Nothing, _      ) = Nothing
        ok (_      , _      , Nothing) = Nothing
    in  ok (p', Just q, r)





-- improve!  merge with orient?
findCyclicOrderWith :: Ord a => [a] -> [a] -> (a -> a -> Bool) -> Maybe [a]
findCyclicOrderWith ns comp f =
  let order = nubOrd $ filter (\x -> any (f x) ns) (comp ++ comp)
  in  if length order == length (nubOrd ns) then Just order else Nothing

findCyclicOrderEdges :: [Arc] -> [(Point, Point)] -> Maybe [(Point, Point)]
findCyclicOrderEdges arcs edges' = findCyclicOrderWith
  (fmap (\(Arc p q) -> (p, q)) arcs)
  edges'
  unorient
  where unorient (p, q) (r, s) = (p == r && q == s) || (p == s && q == r)

-- TODO: replace all with the intervals stuff on oriented components!

-- one issue with this is that you can't wrap around a component more than once
reachables :: Ord a => [a] -> AdjacencyMap a -> Bool
reachables []       _      = True
reachables (a : as) theMap = go a a as theMap
 where
  go :: Ord a => a -> a -> [a] -> AdjacencyMap a -> Bool
  go _ _ [] _ = True
  go firstOne lastOne (b : bs) theMap'
    | b == lastOne || b == firstOne = go firstOne lastOne bs theMap'
    | b `elem` reachable lastOne theMap' = go
      firstOne
      b
      bs
      (removeVertices (fst $ takeTil (reachable lastOne theMap') b) theMap')
    | otherwise = False


removeVertices :: Ord a => [a] -> AdjacencyMap a -> AdjacencyMap a
removeVertices as = compose (fmap removeVertex as)
-- CAREFUL: COUNT PERMUTATIONS WE'VE COUNTED TO ACCOUNT FOR ALL!
-- only works on oriented configuration
--
-- want m|>|m'
--      n|>|n'
-- so you see m m' n' n
fixD :: PD -> Decoration -> Decoration
fixD pd' d@(Decoration arc@(Arc p q) arc'@(Arc p' q'))
  | (p, q) `elem` edgeList pd' = if (p', q') `elem` edgeList pd'
    then d
    else Decoration arc (Arc q' p')
  | (p', q') `elem` edgeList pd' = Decoration (Arc q p) arc'
  | otherwise = Decoration (Arc q p) (Arc q' p')
sameDirection :: PD -> Decoration -> Decoration -> Bool -- is there a second condition?  see interlaced
sameDirection pd d d' = sameDirection' d d' pd || sameDirection' d' d pd
 where
  sameDirection' (Decoration (Arc p q) (Arc p' q')) (Decoration (Arc r s) (Arc r' s')) pd'
    = let list1 = [p, q, p', q', r', s', r, s]
          list2 = [p, q, r, s, r', s', p', q']
      in  reachables list1 pd' || reachables list2 pd'


--want
--  m|>|m'
-- n'|<|n
-- so you see m m' n n'
oppositeDecs :: PD -> Decoration -> Decoration -> Bool
oppositeDecs pd d d' = opposite' d d' pd || opposite' d' d pd
 where
  opposite' :: Decoration -> Decoration -> PD -> Bool -- is there a second condition?  see interlaced
  opposite' (Decoration (Arc p q) (Arc p' q')) (Decoration (Arc r s) (Arc r' s')) pd'
    = let list1 = [p, q, p', q', r, s, r', s']
          list2 = [p, q, r', s', r, s, p', q']
      in  reachables list1 pd' || reachables list2 pd'

  --   case findCyclicOrderEdges [m, m', n, n'] (edgeList (searchPD (== p) pd)) of
  --     Just order ->
  --       liftA2 (||) ([m, m', n, n'] `elem`) ([n', n, m', m] `elem`)
  --         . fmap (fmap (uncurry Arc)) . cyclicPermutations
  --         $ order
  --     Nothing -> False

data DecJordan = Inside | Outside deriving (Eq, Ord, Show)

arrowJordan :: Decoration -> PD -> DecJordan
arrowJordan dec@(Decoration (Arc p p') (Arc q _)) pd =
  case getDecDirection dec of
    Vertical ->
      if even
           . length
           . S.filter
               (\(r, s) ->
                 (  (_x r == _x p && _x s == _x p')
                   || (_x r == _x p' && _x s == _x p)
                   ) -- parallel
                   && _y r
                   >  min (_y p) (_y q)
               ) -- "below"
           . edgeSet
           $ pd
        then Outside
        else Inside
    Horizontal ->
      if even
           . length
           . S.filter
               (\(r, s) ->
                 (  (_y r == _y p && _y s == _y p')
                   || (_y r == _y p' && _y s == _y p)
                   ) -- parallel
                   && _x r
                   >  min (_x p) (_x q)
               ) -- "to the right"
           . edgeSet
           $ pd
        then Outside
        else Inside
    NeitherBad ->
      error
        $ "Tried to determine arrowJordan for decoration which is neither Vertical nor Horizontal: "
        ++ show dec

data CircleJordan = InsideC | OutsideC deriving (Eq, Ord, Show)

-- a `checkComponent` b checks whether a is inside b
checkContainment :: Component -> Component -> CircleJordan
checkContainment comp = checkContainment' (head . S.toList $ comp)
 where
          -- draw a vertical line, i.e. check x coordinates
  checkContainment' :: Point -> Component -> CircleJordan
  checkContainment' p compp =
    if even . S.size $ S.filter (\p' -> _x p' == _x p && _y p' > _y p) compp
      then OutsideC
      else InsideC

dualConfiguration :: Configuration -> Maybe Configuration
dualConfiguration conf =
       flipAllDecorations
      . (\c -> buildConfig
          (S.singleton $ diagram c)
          ( S.fromList
          . catMaybes
          . fmap (`rotateDecoration` conf)
          . S.toList
          $ decos conf
          )
        )
    <$> applyAllDecorations conf

------ Internal?

rotateDecorationCW :: Decoration -> Configuration -> Maybe Decoration
rotateDecorationCW dec@(Decoration (Arc a b) (Arc c d)) config =
  case getDecDirection dec of
    Vertical -> if _y a > _y c
      then if _x a == _x c
        then if _x b > _x a
          then return $ Decoration (Arc a c) (Arc b d)
          else return $ Decoration (Arc b d) (Arc a c)
        else rotateDecoration (Decoration (Arc a b) (Arc d c)) config
      else
        flipDecoration
          <$> rotateDecoration (Decoration (Arc c d) (Arc a b)) config
    Horizontal -> if _x a > _x c
      then if _y a == _y c
        then if _y b > _y a
          then return $ Decoration (Arc b d) (Arc a c)
          else return $ Decoration (Arc a c) (Arc b d)
        else rotateDecoration (Decoration (Arc a b) (Arc d c)) config
      else
        flipDecoration
          <$> rotateDecoration (Decoration (Arc c d) (Arc a b)) config
    NeitherBad ->
      error
        $ "Tried to rotate a decoration which is neither Horizontal nor Vertical: "
        ++ show dec

rotateDecoration :: Decoration -> Configuration -> Maybe Decoration
rotateDecoration = rotateDecorationCW

flipDecoration :: Decoration -> Decoration
flipDecoration (Decoration n n') = Decoration n' n

flipAllDecorations :: Configuration -> Configuration
flipAllDecorations conf = conf { decos = S.map flipDecoration (decos conf) }


