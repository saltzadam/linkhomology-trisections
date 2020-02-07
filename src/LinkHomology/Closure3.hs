module LinkHomology.Closure3 where
import           Core.Tangle             hiding ( Up
                                                , Down
                                                )
import           Core.PlanarDiagram
import           Core.PlanarDiagram.Operations
import           Complex.Hyperbox
import           Core.Grid               hiding ( Orientation )
import           Core.Braids                    ( markovPlats )
import           LinkHomology.LinkHomology
import           LinkHomology.ReidemeisterTwo
import           Algebra.V
import           Algebra.Z2
import           Core.Configuration
import           Control.Arrow                  ( (***) )
import           Core.Util
import qualified Data.Either                   as E
import           Data.Maybe                     ( fromJust
                                                , catMaybes
                                                , fromMaybe
                                                , isJust
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Set                       ( Set )
import           Data.List                      ( sortOn )
import           Control.Monad                  ( replicateM )


-- TODO: add R2 moves (bc right now it's wrong!)
-- TODO: 
closure3'
  :: LinkHomologyTheory
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> Set Labeling
closure3' lht omt1 omt2 omt3 =
  closureMap lht omt1 omt2 omt3 (theta omt1 omt2 omt3)

closure3
  :: LinkHomologyTheory
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> Int
closure3 lht omt1 omt2 omt3 = S.size (closure3' lht omt1 omt2 omt3)

--------- LABELINGS --------------

--  =====
-- t2b  t1
--  V   ^
-- t2   t1b
-- t3b  ||
--  V   ||
-- t3   ||
--  =====

theta
  :: OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> Labeling
theta omt1 omt2 omt3 =
  let comt        = circularOMT omt1 omt2 omt3
      orientedRes = crossOrientNew (tangle comt) (orientation comt)
  in  orientedLabeling orientedRes

----------------- MAPS -------------------------

cludge :: [a] -> (a, a, a)
cludge [x1, x2, x3] = (x1, x2, x3)
cludge _            = error "you promised me a 3-dimensional hyperbox"

unCludge :: (a, a, a) -> [a]
unCludge (x1, x2, x3) = [x1, x2, x3]


closureMap
  :: LinkHomologyTheory
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> LinearMap
closureMap lht omt1 omt2 omt3 =
  diagonalMap (closure3Hyperbox lht omt1 omt2 omt3)

closure3Hyperbox
  :: LinkHomologyTheory
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> Hyperbox (Set Labeling) LinearMap
closure3Hyperbox lht omt1 omt2 omt3 = buildHyperbox
  (getGroupAtDelta . cludge)
  (\(d, e) -> fullConfMap lht (configAtDE (cludge d) (cludge e))
    <<>> hs2 (cludge d) (cludge e)
  )
  [reald1, reald2, reald3]
 where
  configsAndComps
    :: [(((Int, Int, Int), (Z2, Z2, Z2)), (Configuration, [Component]))]
  configsAndComps = fmap
    (graph (\(d', e') -> configAndComps3 d' e' omt1 omt2 omt3))
    [ (cludge d, cludge e)
    | d <- sequence [[0 .. reald1], [0 .. reald2], [0 .. reald3]]
    , e <- sequence [[ZZero, ZOne], [ZZero, ZOne], [ZZero, ZOne]]
    ]
  configAtDE d e = fst . fromJust $ lookup (d, e) configsAndComps
  compsAtDE d e = snd . fromJust $ lookup (d, e) configsAndComps
  diagAtD d = getDiagramAtDelta d omt1 omt2 omt3
  cd = circularDiagram (omtToMt omt1) (omtToMt omt2) (omtToMt omt3)
  (reald1, reald2, reald3) =
    zipWithTuple3 (+) (platLengths3 cd) (crossings3 cd)
  getGroupAtDelta :: (Int, Int, Int) -> Set Labeling
  getGroupAtDelta = S.fromList . buildKhovanovGroup . diagAtD
  hs2 :: (Int, Int, Int) -> (Z2, Z2, Z2) -> Labeling -> Set Labeling
  hs2 d e l = S.singleton $ compose (fmap deleteComp (compsAtDE d e)) l
-- TODO: would it be helpful to define a type like CompilingStuff = CS (Maybe Component) (Maybe (Either Decoration CrossDeco))
configAndComps3
  :: (Int, Int, Int)
  -> (Z2, Z2, Z2)
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> (Configuration, [Component])
configAndComps3 del (ee1, ee2, ee3) omt1 omt2 omt3 =
  ( buildConfig (S.singleton $ diagram . (`deleteComps` comps) $ diagAtD del)
                (S.fromList (decs ++ cdecs))
  , comps
  )
 where
  cd = circularDiagram (omtToMt omt1) (omtToMt omt2) (omtToMt omt3)
  diagAtD d = getDiagramAtDelta d omt1 omt2 omt3
  (d1, d2, d3) = platLengths3 cd
  diag         = diagAtD del
  -- this is basically pattern matching -- could make it more compact but I'm not sure how much better.  actually limiting scope is good
  buildArrows1'
    :: (Int, Int, Int)
    -> Z2
    -> (Maybe Component, Maybe (Either Decoration CrossDeco))
  buildArrows1' (dd1, _, _) e1 = if
    | e1 == ZZero -> (Nothing, Nothing)
    | dd1 < (length . decos1 $ cd) -> (Nothing, Just $ E.Left (decos1 cd !! dd1))
    | otherwise -> ( r2topology (crossDecos1 cd !! (dd1 - d1)) (diagram diag)
       , Just $ E.Right (crossDecos1 cd !! (dd1 - d1))
       )
  buildArrows2'
    :: (Int, Int, Int)
    -> Z2
    -> (Maybe Component, Maybe (Either Decoration CrossDeco))
  buildArrows2' (_, dd2, _) e2 = if
    | e2 == ZZero -> (Nothing, Nothing)
    | dd2 < (length . decos2 $ cd) -> (Nothing, Just $ E.Left (decos2 cd !! dd2))
    | otherwise -> ( r2topology (crossDecos2 cd !! (dd2 - d2)) (diagram diag)
       , Just $ E.Right (crossDecos2 cd !! (dd2 - d2))
       )
  buildArrows3'
    :: (Int, Int, Int)
    -> Z2
    -> (Maybe Component, Maybe (Either Decoration CrossDeco))
  buildArrows3' (_, _, dd3) e3 = if
    | e3 == ZZero -> (Nothing, Nothing)
    | dd3 < (length . decos3 $ cd) -> (Nothing, Just $ E.Left (decos3 cd !! dd3))
    | otherwise -> ( r2topology (crossDecos3 cd !! (dd3 - d3)) (diagram diag)
       , Just $ E.Right (crossDecos3 cd !! (dd3 - d3))
       )
  compile
    :: (Maybe Component, Maybe (Either Decoration CrossDeco))
    -> (Maybe Component, Maybe (Either Decoration CrossDeco))
    -> (Maybe Component, Maybe (Either Decoration CrossDeco))
    -> ([Component], [Decoration], [CrossDeco])
  compile (cs, edc) (cs', edc') (cs'', edc'') =
    let comps' = catMaybes [cs, cs', cs'']
        decs'  = E.lefts . catMaybes $ [edc, edc', edc'']
        cdecs' =
          E.rights
            . catMaybes
            . fmap snd
            . filter (isJust . fst)
            $ [(cs, edc), (cs', edc'), (cs'', edc'')]
    in  (comps', decs', cdecs')
    -- TODO: lol use pattern match
  (comps, decs, cdecs'') = compile (buildArrows1' del ee1)
                                   (buildArrows2' del ee2)
                                   (buildArrows3' del ee3)
  cdecs = fmap (\d -> fst $ uncross d (determineOrientation d)) cdecs''

getDiagramAtDelta
  :: (Int, Int, Int)
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> Configuration
getDiagramAtDelta (dd1, dd2, dd3) omt1 omt2 omt3
  = let
      cd = circularDiagram (omtToMt omt1) (omtToMt omt2) (omtToMt omt3)
      (d1, d2, d3) = platLengths3 cd
      pd =
        crossOrientNew (closureD cd) (orientation (circularOMT omt1 omt2 omt3))
---      
      decs1  = take (min dd1 d1) (decos1 cd)
      cdcs1' = reverse . take (max 0 (dd1 - d1)) . reverse $ crossDecos1 cd
      -- cdcs1 = foldCDCS initialPD cdcs1'
      -- cdcs1  = catMaybes . fmap (`attachCrossDec` pd) $ cdcs1'
---
      decs2  = take (min dd2 d2) (decos2 cd)
      cdcs2' = reverse . take (max 0 (dd2 - d2)) . reverse $ crossDecos2 cd
      -- cdcs2 = foldCDCS initialPD cdcs2'
      -- cdcs2  = catMaybes . fmap (`attachCrossDec` pd) $ cdcs2'
---
      decs3  = take (min dd3 d3) (decos3 cd)
      cdcs3' = reverse . take (max 0 (dd3 - d3)) . reverse $ crossDecos3 cd
      -- cdcs3  = catMaybes . fmap (`attachCrossDec` pd) $ cdcs3'
      -- cdcs3 = foldCDCS initialPD cdcs3'
---
      --initialPD =        diagram . fromJust . applyAllDecorations $ buildConfig (S.singleton pd) (S.fromList (decos1 cd ++ decos2 cd ++ decos3 cd))

      --comps'' = catMaybes . fmap (`r2topology` pd') $ (cdcs3' ++ cdcs2' ++ cdcs1')
        --TODO: why not just make decorationAction into longOneHandleAttachment?
      longAttach =
        kleisliCompose (fmap r2HandleAttachment (cdcs1' ++ cdcs2' ++ cdcs3'))
      --configPreR2' = buildConfig (S.singleton pd) (S.fromList (decs1 ++ decs2 ++ decs3))
      -- pd' = fromJust . fmap diagram . applyAllDecorations $ configPreR2'
              -- TODO: why do we apply applyAllDecorations twice??
    in
      flip buildConfig S.empty
      . S.singleton
      . fromMaybe (error "error here!")
      . longAttach
      . diagram
      -- . (`deleteComps` comps'')
      . fromJust
      . applyAllDecorations
      $ buildConfig (S.singleton pd) (S.fromList (decs1 ++ decs2 ++ decs3))

hyperboxOfConfigurations
  :: OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> Map (Delta, Epsilon) Configuration
hyperboxOfConfigurations omt1 omt2 omt3 =
  M.mapKeys (unCludge *** unCludge)
    . graphMap (\(d, e) -> fst $ configAndComps3 d e omt1 omt2 omt3)
    $ delsAndEps
 where
  (reald1, reald2, reald3) =
    zipWithTuple3 (+) (platLengths3 cd) (crossings3 cd)
  realDs =
    [ (d1, d2, d3)
    | d1 <- [0 .. reald1]
    , d2 <- [0 .. reald2]
    , d3 <- [0 .. reald3]
    ]
  cd  = circularDiagram (omtToMt omt1) (omtToMt omt2) (omtToMt omt3)
  eps = cludge <$> replicateM 3 [ZZero, ZOne]
  delsAndEps =
    [ (d, e)
    | d@(d1, d2, d3) <- realDs
    , e@(e1, e2, e3) <- eps
    , ([d1, d2, d3] `addDE` [e1, e2, e3]) `lessThanEQ` [reald1, reald2, reald3]
    ]
  lessThanEQ :: [Int] -> [Int] -> Bool
  lessThanEQ = (and .) . zipWith (<=)



------------- DIAGRAMS ----------------
-- have t1, t2, t3
-- put them as
--  =====
-- t2b  t1
--  V   ^
-- t2   t1b
-- t3b  ||
--  V   ||
-- t3   ||
--  =====

-- p2   p1
-- p3

data Closure3Diagram = Closure3Diagram {closureD :: CrossPD,
                                        decos1 :: [Decoration],
                                        decos2 :: [Decoration],
                                        decos3 :: [Decoration],
                                        crossDecos1 :: [CrossDeco],
                                        crossDecos2 :: [CrossDeco],
                                        crossDecos3 :: [CrossDeco]} deriving (Eq, Ord, Show)

platLengths3 :: Closure3Diagram -> (Int, Int, Int)
platLengths3 cd =
  (length . decos1 $ cd, length . decos2 $ cd, length . decos3 $ cd)

crossings3 :: Closure3Diagram -> (Int, Int, Int)
crossings3 cd =
  ( length . crossDecos1 $ cd
  , length . crossDecos2 $ cd
  , length . crossDecos3 $ cd
  )

circularOMT
  :: OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
circularOMT omt1 omt2 omt3 = OMTangle
  (closureD $ circularDiagram (omtToMt omt1) (omtToMt omt2) (omtToMt omt3))
  []
  os
 where
  strands = pointsOnBottom . omtToTangle $ omt1
  o2'     = M.mapKeys (shiftYBy (tangleBottom (omtToTangle omt2) + strands + 1))
                      (orientation omt2)
  o1' =
    M.mapKeys (shiftXBy strands . shiftYBy strands)
      . orientation
      . rotateOMT
      $ omt1
  o3' = M.mapKeys
    (shiftYBy
      ( strands
      + 2
      * tangleBottom (omtToTangle omt2)
      + 1
      + tangleBottom (omtToTangle omt3)
      + 1
      )
    )
    (orientation omt3)
  os = M.unions [o1', o2', o3']

circularDiagram :: MorseTangle -> MorseTangle -> MorseTangle -> Closure3Diagram-- should be Closure3Diagram
circularDiagram t1 t2 t3 = Closure3Diagram fullDiagram d1 d2 d3 cd1 cd2 cd3
 where
  strands     = pointsOnBottom . mtangle $ t1
  t1bar       = mirrorMorse t1
  t2bar       = mirrorMorse t2
  t3bar       = mirrorMorse t3
  twoColumns  = leftColumn `sideBySide` rightColumn
  leftColumn' = disjointUnion
    [ mtangle t2bar
    , mtangle t2
    , onPoints (shiftYBy (-1))
    . mtangle
    $ t3bar -- disjoint union is not for gluing!  so need a shift
    , mtangle t3
    ]
  rightColumn' =
    disjointUnion [rotateT . mtangle $ t1, rotateT . mtangle $ t1bar]
  rBottom      = tangleBottom rightColumn'
  lBottom      = tangleBottom leftColumn'
  leftColumn   = extendBy (rBottom - lBottom) leftColumn'
  rightColumn  = extendBy (lBottom - rBottom) rightColumn'
  columnLength = tangleBottom leftColumn
  fullDiagram =
    upsideDown (markovPlats strands)
      ++ onPoints (shiftYBy (strands - 1))                twoColumns
      ++ onPoints (shiftYBy (strands + columnLength - 1)) (markovPlats strands)
  disjointUnion = foldl dUnion []
  d `sideBySide` d' = d ++ onPoints (shiftXBy strands) d'
  extendBy :: Int -> CrossPD -> CrossPD
  extendBy k cpd = if k <= 0
    then cpd
    else cpd ++ fmap
      (fmap (shiftYBy (tangleBottom cpd)))
      [ CrossJoin (Point i j) (Point i (j + 1))
      | i <- [0 .. strands - 1]
      , j <- [0 .. k - 1]
      ]

  p1' =
    onPoints (shiftXBy strands . shiftYBy strands) . mplats $ mirrorMorse t1
  p1bar' = onPoints (shiftYBy 1) p1'
  p2bar' = onPoints (shiftYBy strands) . mplats $ mirrorMorse t2
  p2'    = onPoints (shiftYBy 1) p2bar'
  p3bar' =
    fmap (fmap (shiftYBy (2 * tangleBottom (mtangle t2) + strands + 1)))
      . mplats
      $ mirrorMorse t3
  p3' = onPoints (shiftYBy 1) p3bar'
  d1  = zipWith platDecorations p1bar' p1'
  d2  = zipWith platDecorations p2bar' p2'
  d3  = zipWith platDecorations p3bar' p3'
  cd1 =
    let cd11 =
          reverse
            . shift11
            . sortOn (maximum . _yNode)
            . filter isCross
            . rotateT
            . mtangle
            $ t1
        cd12 =
          shift12
            . sortOn (minimum . _yNode)
            . onPoints (shiftYBy (tangleBottom (mtangle t1) + 1))
            . filter isCross
            . rotateT
            . mtangle
            $ t1bar
    in    -- reverse $ 
        reverse $ zipWith CrossDeco' cd11 cd12
  cd2 =
    let cd21 =
          reverse
            . shift21
            . sortOn (minimum . _yNode)
            . filter isCross
            . mtangle
            $ t2bar -- sort puts the lowest-y-value first
        cd22 =
          shift22
            . sortOn (minimum . _yNode)
            . onPoints (shiftYBy (tangleBottom (mtangle t2bar) + 1))
            . filter isCross
            . mtangle
            $ t2
    in  reverse $ zipWith CrossDeco' cd21 cd22
  cd3 =
    let cd31 =
          reverse
            . shift31
            . sortOn (maximum . _yNode)
            . filter isCross
            . mtangle
            $ t3bar
        cd32 =
          shift32
            . sortOn (minimum . _yNode)
            . onPoints (shiftYBy (tangleBottom (mtangle t3bar) + 1))
            . filter isCross
            . mtangle
            $ t3
    in  reverse $ zipWith CrossDeco' cd31 cd32
  shift11 = onPoints (shiftYBy strands . shiftXBy strands)
  shift12 = onPoints (shiftYBy strands . shiftXBy strands)
  shift21 = onPoints (shiftYBy strands)
  shift22 = onPoints (shiftYBy strands)
  shift31 = onPoints
    ( shiftYBy strands
    . shiftYBy (tangleBottom (mtangle t2bar `dUnion` mtangle t2))
    )
  shift32 = shift31

---- triple product stuff

-- just use the same hyperbox

tripleProductClosure
  :: LinkHomologyTheory
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> LinearMap
tripleProductClosure lht omt1 omt2 omt3 = diagonal23Then1
  (closure3Hyperbox lht omt1 omt2 omt3)
 where
  diagonal23Then1 :: Ring m => Hyperbox o m -> m
  diagonal23Then1 h = edge <<>> diagonal
   where
    cube     = arrowsH (compress h)
    diagonal = (M.! ([0, 0, 0], [ZZero, ZOne, ZOne])) cube
    edge     = (M.! ([0, 1, 1], [ZOne, ZZero, ZZero])) cube

tripleProductClosures
  :: LinkHomologyTheory
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> LinearMap
tripleProductClosures lht omt1 omt2 omt3 = rconcat
  [diagonal23Then1, diagonal13Then2, diagonal12Then3]
 where
  diagonal23Then1 = edge1 <<>> diagonal23
  diagonal13Then2 = edge2 <<>> diagonal13
  diagonal12Then3 = edge3 <<>> diagonal12
  cube            = arrowsH (compress (closure3Hyperbox lht omt1 omt2 omt3))
  diagonal23      = (M.! ([0, 0, 0], [ZZero, ZOne, ZOne])) cube
  diagonal13      = (M.! ([0, 0, 0], [ZOne, ZZero, ZOne])) cube
  diagonal12      = (M.! ([0, 0, 0], [ZOne, ZOne, ZZero])) cube
  edge1           = (M.! ([0, 1, 1], [ZOne, ZZero, ZZero])) cube
  edge2           = (M.! ([1, 0, 1], [ZZero, ZOne, ZZero])) cube
  edge3           = (M.! ([1, 1, 0], [ZZero, ZZero, ZOne])) cube

tripleProducts' :: LinkHomologyTheory -> OrientedMorseTangle ->   OrientedMorseTangle -> OrientedMorseTangle -> Set Labeling
tripleProducts' lht omt1 omt2 omt3 =
  tripleProductClosures lht omt1 omt2 omt3 (theta omt1 omt2 omt3)
tripleProducts :: LinkHomologyTheory -> OrientedMorseTangle ->   OrientedMorseTangle -> OrientedMorseTangle -> Int
tripleProducts lht omt1 omt2 omt3 =
  S.size (tripleProducts' lht omt1 omt2 omt3)
