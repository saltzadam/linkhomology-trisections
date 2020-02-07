module LinkHomology.Closure1 where
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
                                                , isJust
                                                , catMaybes
                                                , fromMaybe
                                                )
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Set                       ( Set )
import           Data.List                      ( sortOn )

closure1' :: LinkHomologyTheory -> OrientedMorseTangle -> Set Labeling
closure1' lht omt = closureMap lht omt (theta omt)

closure1 :: LinkHomologyTheory -> OrientedMorseTangle -> Int
closure1 lht omt = S.size (closure1' lht omt)

--------- LABELINGS --------------

--  =====
-- t2b  t1
--  V   ^
-- t2   t1b
--  =====

theta :: OrientedMorseTangle -> Labeling
theta omt =
  let comt        = circularOMT omt
      orientedRes = crossOrientNew (tangle comt) (orientation comt)
  in  orientedLabeling orientedRes

----------------- MAPS -------------------------

cludge :: [a] -> (a, a)
cludge [x1, x2] = (x1, x2)
cludge _        = error "you promised me a 2-dimensional hyperbox"

inverseCludge :: (a, a) -> [a]
inverseCludge (x1, x2) = [x1, x2]


closureMap :: LinkHomologyTheory -> OrientedMorseTangle -> LinearMap
closureMap lht omt = diagonalMap (closure1Hyperbox lht omt)

-- TODO:  remove head
closure1Hyperbox
  :: LinkHomologyTheory
  -> OrientedMorseTangle
  -> Hyperbox (Set Labeling) LinearMap
closure1Hyperbox lht omt = buildHyperbox
  (getGroupAtDelta . head)
  (\(d, e) -> fullConfMap lht (configAtDE (head d) (head e))
    <<>> hs2 (head d) (head e)
  )
  [reald]
 where
  configsAndComps :: [((Int, Z2), (Configuration, [Component]))]
  configsAndComps = fmap
    (graph (\(d', e') -> configAndComps1 d' e' omt))
    [ (head d, head e)
    | d <- sequence [[0 .. reald]]
    , e <- sequence [[ZZero, ZOne]]
    ]
  compsAtDE d e = snd . fromJust $ lookup (d, e) configsAndComps
  configAtDE d e = fst . fromJust $ lookup (d, e) configsAndComps
  cd    = circularDiagram (omtToMt omt)
  reald = platLengths cd + crossings cd
  getGroupAtDelta :: Int -> Set Labeling
  getGroupAtDelta = S.fromList . buildKhovanovGroup . diagAtD
  diagAtD d = getDiagramAtDelta d omt
  hs2 d e l = S.singleton $ compose (fmap deleteComp (compsAtDE d e)) l
-- TODO: would it be helpful to define a type like CompilingStuff = CS (Maybe Component) (Maybe (Either Decoration CrossDeco))
configAndComps1
  :: Int -> Z2 -> OrientedMorseTangle -> (Configuration, [Component])
configAndComps1 del ee omt =
  ( buildConfig (S.singleton . diagram . (`deleteComps` comps) $ diagAtD del)
                (S.fromList (decs ++ cdecs))
  , comps
  )
 where
  cd = circularDiagram (omtToMt omt)
  diagAtD d = getDiagramAtDelta d omt
  diag = diagAtD del
  d1   = platLengths cd
  buildArrows
    :: Int -> Z2 -> (Maybe Component, Maybe (Either Decoration CrossDeco))
  buildArrows dd1 e1 = if
    | e1 == ZZero
    -> (Nothing, Nothing)
    | dd1 < (length . decos1 $ cd)
    -> (Nothing, Just $ E.Left (decos1 cd !! dd1))
    | otherwise
    -> ( r2topology (crossDecos1 cd !! (dd1 - d1)) (diagram diag)
       , Just $ E.Right (crossDecos1 cd !! (dd1 - d1))
       )
  compile
    :: (Maybe Component, Maybe (Either Decoration CrossDeco))
    -> ([Component], [Decoration], [CrossDeco])
  compile (cs, edc) =
    let
      comps' = catMaybes [cs]
      decs'  = E.lefts . catMaybes $ [edc]
      cdecs' =
        E.rights . catMaybes . fmap snd . filter (isJust . fst) $ [(cs, edc)]
    in
      (comps', decs', cdecs')
    -- TODO: lol use pattern match
  (comps, decs, cdecs'') = compile (buildArrows del ee)
  cdecs = fmap (\d -> fst $ uncross d (determineOrientation d)) cdecs''

getDiagramAtDelta :: Int -> OrientedMorseTangle -> Configuration
getDiagramAtDelta dd1 omt =
  let cd           = circularDiagram (omtToMt omt)
      d1           = platLengths cd
      pd = crossOrientNew (closureD cd) (orientation (circularOMT omt))
  ---      
      decs1        = take (min dd1 d1) (decos1 cd)
      cdcs1'       = take (max 0 (dd1 - d1)) $ crossDecos1 cd
      cdcs1        = foldCDCS initialPD cdcs1'
  ---
      comps''      = catMaybes . fmap (`r2topology` pd') $ cdcs1'
      initialPD =        diagram . fromJust . applyAllDecorations $ buildConfig (S.singleton pd) (S.fromList (decos1 cd))
        --TODO: put orientConfigCCW into applyAllDecorations?
        --TODO: why not just make decorationAction into longOneHandleAttachment?
      longAttach   = kleisliCompose (fmap longOneHandleAttachment cdcs1)
      configPreR2' = buildConfig (S.singleton pd) (S.fromList decs1)
      pd' = fromJust . fmap diagram . applyAllDecorations $ configPreR2'
            -- TODO: why do we apply applyAllDecorations twice??
  in  flip buildConfig S.empty
      . S.singleton
      . fromMaybe (error "error here!")
      . longAttach
      . diagram
      . (`deleteComps` comps'')
      . fromJust
      . applyAllDecorations
      $ buildConfig (S.singleton pd) (S.fromList decs1)

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
data ClosureDiagram = ClosureDiagram {closureD :: CrossPD,
                                        decos1 :: [Decoration],
                                        crossDecos1 :: [CrossDeco]
                                       } deriving (Eq, Ord, Show)

platLengths :: ClosureDiagram -> Int
platLengths cd = length . decos1 $ cd

crossings :: ClosureDiagram -> Int
crossings cd = length . crossDecos1 $ cd

circularOMT :: OrientedMorseTangle -> OrientedMorseTangle
circularOMT omt = OMTangle (closureD $ circularDiagram (omtToMt omt)) [] os
 where
  strands = pointsOnBottom . omtToTangle $ omt
  o1' =
    M.mapKeys (shiftXBy strands . shiftYBy strands)
      . orientation
      . rotateOMT
      $ omt
  os = o1'

circularDiagram :: MorseTangle -> ClosureDiagram -- should be Closure3Diagram
circularDiagram t = ClosureDiagram fullDiagram d1 cd1
 where
  strands      = pointsOnBottom . mtangle $ t
  tbar         = mirrorMorse t
  twoColumns   = leftColumn `sideBySide` rightColumn
  leftColumn'  = disjointUnion [mtangle tbar, mtangle t]
  rightColumn' = []
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

  p1' = onPoints (shiftXBy strands . shiftYBy strands) . mplats $ mirrorMorse t
  p1bar' = onPoints (shiftYBy 1) p1'
  d1 = zipWith platDecorations p1bar' p1'
  cd1 =
    let cd11 =
          reverse
            . shift11
            . sortOn (maximum . _yNode)
            . filter isCross
            . rotateT
            . mtangle
            $ t
        cd12 =
          shift12
            . sortOn (minimum . _yNode)
            . onPoints (shiftYBy (tangleBottom (mtangle t) + 1))
            . filter isCross
            . rotateT
            . mtangle
            $ tbar
    in     -- reverse $ 
        zipWith CrossDeco' cd11 cd12
  shift11 = onPoints (shiftYBy strands . shiftXBy strands)
  shift12 = onPoints (shiftYBy strands . shiftXBy strands)
