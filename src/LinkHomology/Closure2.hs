module LinkHomology.Closure2 where
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
import           Core.Util
import qualified Data.Either                   as E
import           Data.Maybe                     ( fromJust
                                                , catMaybes
                                                , fromMaybe
                                                , isJust
                                                )
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Set                       ( Set )
import           Data.List                      ( sortOn )

doubles
  :: LinkHomologyTheory
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> [Int]
doubles lht omt1 omt2 omt3 = S.size <$> doubles' lht omt1 omt2 omt3

doubles'
  :: LinkHomologyTheory
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> [Set Labeling]
doubles' lht omt1 omt2 omt3 =
  [closure2' lht omt1 omt2, closure2' lht omt2 omt3, closure2' lht omt3 omt1]


closure2'
  :: LinkHomologyTheory
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> Set Labeling
closure2' lht omt1 omt2 = closureMap lht omt1 omt2 (theta omt1 omt2)

closure2
  :: LinkHomologyTheory -> OrientedMorseTangle -> OrientedMorseTangle -> Int
closure2 lht omt1 omt2 = S.size (closure2' lht omt1 omt2)

--------- LABELINGS --------------

--  =====
-- t2b  t1
--  V   ^
-- t2   t1b
--  =====

theta :: OrientedMorseTangle -> OrientedMorseTangle -> Labeling
theta omt1 omt2 =
  let comt        = circularOMT omt1 omt2
      orientedRes = crossOrientNew (tangle comt) (orientation comt)
  in  orientedLabeling orientedRes

----------------- MAPS -------------------------

cludge :: [a] -> (a, a)
cludge [x1, x2] = (x1, x2)
cludge _        = error "you promised me a 2-dimensional hyperbox"

inverseCludge :: (a, a) -> [a]
inverseCludge (x1, x2) = [x1, x2]


closureMap
  :: LinkHomologyTheory
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> LinearMap
closureMap lht omt1 omt2 = diagonalMap (closure2Hyperbox lht omt1 omt2)

closure2Hyperbox
  :: LinkHomologyTheory
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> Hyperbox (Set Labeling) LinearMap
closure2Hyperbox lht omt1 omt2 = buildHyperbox
  (getGroupAtDelta . cludge)
  (\(d, e) -> fullConfMap lht (configAtDE (cludge d) (cludge e))
    <<>> hs2 (cludge d) (cludge e)
  )
  [reald1, reald2]
 where
  configsAndComps :: [(((Int, Int), (Z2, Z2)), (Configuration, [Component]))]
  configsAndComps = fmap
    (graph (\(d', e') -> configAndComps2 d' e' omt1 omt2))
    [ (cludge d, cludge e)
    | d <- sequence [[0 .. reald1], [0 .. reald2]]
    , e <- sequence [[ZZero, ZOne], [ZZero, ZOne]]
    ]
  configAtDE d e = fst . fromJust $ lookup (d, e) configsAndComps
  compsAtDE d e = snd . fromJust $ lookup (d, e) configsAndComps
  diagAtD d = getDiagramAtDelta d omt1 omt2
  cd               = circularDiagram (omtToMt omt1) (omtToMt omt2)
  (reald1, reald2) = zipWithTuple2 (+) (platLengths2 cd) (crossings2 cd)
  getGroupAtDelta :: (Int, Int) -> Set Labeling
  getGroupAtDelta = S.fromList . buildKhovanovGroup . diagAtD
  hs2 :: (Int, Int) -> (Z2, Z2) -> Labeling -> Set Labeling
  hs2 d e l = S.singleton $ compose (fmap deleteComp (compsAtDE d e)) l

--   (getGroupAtDelta . cludge)
--   (\(d, e) ->
--     fullConfMap lht (fst $ configAndComps2 (cludge d) (cludge e) omt1 omt2)
--       <<>> hs2 (cludge d) (cludge e)
--   )
--   [reald1, reald2]
--  where
--   cd               = circularDiagram (omtToMt omt1) (omtToMt omt2)
--   (reald1, reald2) = zipWithTuple2 (+) (platLengths2 cd) (crossings2 cd)
--   getGroupAtDelta :: (Int, Int) -> Set Labeling
--   getGroupAtDelta = S.fromList . buildKhovanovGroup . diagAtD
--   diagAtD d = getDiagramAtDelta d omt1 omt2
--   hs2 del (ee1, ee2) l = S.singleton $ compose
--     (fmap deleteComp $ snd $ configAndComps2 del (ee1, ee2) omt1 omt2)
--     l
-- -
 -- TODO: would it be helpful to define a type like CompilingStuff = CS (Maybe Component) (Maybe (Either Decoration CrossDeco))
configAndComps2
  :: (Int, Int)
  -> (Z2, Z2)
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> (Configuration, [Component])
configAndComps2 del (ee1, ee2) omt1 omt2 =
  ( buildConfig (S.singleton . diagram . (`deleteComps` comps) $ diagAtD del)
                (S.fromList (decs ++ cdecs))
  , comps
  )
 where
  cd = circularDiagram (omtToMt omt1) (omtToMt omt2)
  diagAtD d = getDiagramAtDelta d omt1 omt2
  diag     = diagAtD del
  (d1, d2) = platLengths2 cd
  -- this is basically pattern matching -- could make it more compact but I'm not sure how much better.  actually limiting scope is good
  buildArrows1'
    :: (Int, Int)
    -> Z2
    -> (Maybe Component, Maybe (Either Decoration CrossDeco))
  buildArrows1' (dd1, _) e1 = if
    | e1 == ZZero -> (Nothing, Nothing)
    | dd1 < (length . decos1 $ cd) -> (Nothing, Just $ E.Left (decos1 cd !! dd1))
    | otherwise -> ( r2topology (crossDecos1 cd !! (dd1 - d1)) (diagram diag)
       , Just $ E.Right (crossDecos1 cd !! (dd1 - d1))
       )
  buildArrows2'
    :: (Int, Int)
    -> Z2
    -> (Maybe Component, Maybe (Either Decoration CrossDeco))
  buildArrows2' (_, dd2) e2 = if
    | e2 == ZZero -> (Nothing, Nothing)
    | dd2 < (length . decos2 $ cd) -> (Nothing, Just $ E.Left (decos2 cd !! dd2))
    | otherwise -> ( r2topology (crossDecos2 cd !! (dd2 - d2)) (diagram diag)
       , Just $ E.Right (crossDecos2 cd !! (dd2 - d2))
       )
  compile
    :: (Maybe Component, Maybe (Either Decoration CrossDeco))
    -> (Maybe Component, Maybe (Either Decoration CrossDeco))
    -> ([Component], [Decoration], [CrossDeco])
  compile (cs, edc) (cs', edc') =
    let comps' = catMaybes [cs, cs']
        decs'  = E.lefts . catMaybes $ [edc, edc']
        cdecs' =
          E.rights
            . catMaybes
            . fmap snd
            . filter (isJust . fst)
            $ [(cs, edc), (cs', edc')]
    in  (comps', decs', cdecs')
    -- TODO: lol use pattern match
  (comps, decs, cdecs'') =
    compile (buildArrows1' del ee1) (buildArrows2' del ee2)
  cdecs = fmap (\d -> fst $ uncross d (determineOrientation d)) cdecs''

getDiagramAtDelta
  :: (Int, Int) -> OrientedMorseTangle -> OrientedMorseTangle -> Configuration
getDiagramAtDelta (dd1, dd2) omt1 omt2 =
  let
    cd           = circularDiagram (omtToMt omt1) (omtToMt omt2)
    (d1, d2)     = platLengths2 cd
    pd = crossOrientNew (closureD cd) (orientation (circularOMT omt1 omt2))
---      
    decs1        = take (min dd1 d1) (decos1 cd)
    cdcs1'       = reverse . take (max 0 (dd1 - d1))  . reverse $ crossDecos1 cd
    -- cdcs1        = foldCDCS initialPD cdcs1'
---
    decs2        = take (min dd2 d2) (decos2 cd)
    cdcs2'       = reverse . take (max 0 (dd2 - d2)) . reverse $ crossDecos2 cd
    -- cdcs2        = foldCDCS initialPD cdcs2'
---
    -- initialPD =        diagram . fromJust . applyAllDecorations $ buildConfig (S.singleton pd) (S.fromList (decos1 cd ++ decos2 cd))
    -- comps''      = catMaybes . fmap (`r2topology` pd') $ (cdcs2' ++ cdcs1')
      --TODO: put orientConfigCCW into applyAllDecorations?
      --TODO: why not just make decorationAction into longOneHandleAttachment?
    longAttach = kleisliCompose (fmap r2HandleAttachment (cdcs1' ++ cdcs2'))
    -- configPreR2' = buildConfig (S.singleton pd) (S.fromList (decs1 ++ decs2))
    -- pd'          = fromJust . fmap diagram . applyAllDecorations $ configPreR2'
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
    $ buildConfig (S.singleton pd) (S.fromList (decs1 ++ decs2))

-- hyperboxOfConfigurations
--   :: OrientedMorseTangle
--   -> OrientedMorseTangle
--   -> Map (Delta, Epsilon) Configuration
-- hyperboxOfConfigurations omt1 omt2 =
--   M.mapKeys (inverseCludge *** inverseCludge)
--     . graphMap (\(d, e) -> fst $ configAndComps2 d e omt1 omt2)
--     $ delsAndEps
--  where
--   (reald1, reald2) = zipWithTuple2 (+) (platLengths2 cd) (crossings2 cd)
--   realDs           = [ [d1, d2] | d1 <- [0 .. reald1], d2 <- [0 .. reald2] ]
--   cd               = circularDiagram (omtToMt omt1) (omtToMt omt2)
--   eps              = replicateM 2 [ZZero, ZOne]
--   delsAndEps       = fmap
--     (cludge *** cludge)
--     [ (d, e)
--     | d <- realDs
--     , e <- eps
--     , (!! 0) (d `addDE` e) <= reald1
--     , (!! 1) (d `addDE` e) <= reald2
--     ]



------------- DIAGRAMS ----------------
data Closure2Diagram = Closure2Diagram {closureD :: CrossPD,
                                        decos1 :: [Decoration],
                                        decos2 :: [Decoration],
                                        crossDecos1 :: [CrossDeco],
                                        crossDecos2 :: [CrossDeco]
                                       } deriving (Eq, Ord, Show)

platLengths2 :: Closure2Diagram -> (Int, Int)
platLengths2 cd = (length . decos1 $ cd, length . decos2 $ cd)

crossings2 :: Closure2Diagram -> (Int, Int)
crossings2 cd = (length . crossDecos1 $ cd, length . crossDecos2 $ cd)

circularOMT :: OrientedMorseTangle -> OrientedMorseTangle -> OrientedMorseTangle
circularOMT omt1 omt2 = OMTangle
  (closureD $ circularDiagram (omtToMt omt1) (omtToMt omt2))
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
  os = M.unions [o1', o2']

circularDiagram :: MorseTangle -> MorseTangle -> Closure2Diagram-- should be Closure3Diagram
circularDiagram t1 t2 = Closure2Diagram fullDiagram d1 d2 cd1 cd2
 where
  strands     = pointsOnBottom . mtangle $ t1
  t1bar       = mirrorMorse t1
  t2bar       = mirrorMorse t2
  twoColumns  = leftColumn `sideBySide` rightColumn
  leftColumn' = disjointUnion [mtangle t2bar, mtangle t2]
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
  d1     = zipWith platDecorations p1bar' p1'
  d2     = zipWith platDecorations p2bar' p2'
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
    in     -- reverse $ 
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
    in     -- reverse $
       reverse $ zipWith CrossDeco' cd21 cd22
  shift11 = onPoints (shiftYBy strands . shiftXBy strands)
  shift12 = onPoints (shiftYBy strands . shiftXBy strands)
  shift21 = onPoints (shiftYBy strands)
  shift22 = onPoints (shiftYBy strands)
