module Complex.ClosedInvariant where
import           Core.Tangle
import           Core.PlanarDiagram
import           Complex.Hyperbox
import           Core.Grid               hiding ( Orientation )
import           Core.Braids                    ( markovPlats )
-- import           LinkHomology.Multiplication
import           LinkHomology.LinkHomology
import           Algebra.V
import           Algebra.Z2
import           Core.Configuration
-- import Core.CubeUtil
import Data.List.Extra (foldl')
import Core.CubeUtil

-- import           Data.List                      ( sortBy )
import           Data.Maybe                     ( fromJust )
import           Data.Map.Strict                       ( Map )
import qualified Data.Map.Strict                      as M
import qualified Data.Set                      as S
import           Data.Set                       ( Set )

type Output = Int

closedInvariant
  :: OrientedMorseTangle -> OrientedMorseTangle -> OrientedMorseTangle -> Output
closedInvariant = undefined

-- TODO: add R2 moves (bc right now it's wrong!)
-- TODO: 
closureInvariant
  :: LinkHomologyTheory
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> Set Labeling
closureInvariant lht omt1 omt2 omt3 = foldl' (addSetsMod2) S.empty $  S.toList $ S.map
  (closureMap lht omt1 omt2 omt3)
  (orientedInvariantLabelings omt1 omt2 omt3)

--------- LABELINGS --------------

orientedFlatCircularDiagram'
  :: OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> (ClosureDiagram, Orientation)
orientedFlatCircularDiagram' omt1 omt2 omt3 =
  let oomt1 = orientedResolution omt1
      oomt2 = orientedResolution omt2
      oomt3 = orientedResolution omt3
  in  ( flatCircularDiagram oomt1 oomt2 oomt3
      , M.unions . fmap orientation $ [omt1, omt2, omt3]
      )

orientedInvariantLabelings
  :: OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> Set Labeling
orientedInvariantLabelings omt1 omt2 omt3 =
  let (fcd, o)   = orientedFlatCircularDiagram' omt1 omt2 omt3
      pd         = unsafeCrossToFlat . closureD $ fcd
      orientedPD = orientConsistently pd o
  in  orientedLabeling orientedPD


----------------- MAPS -------------------------


closureMap
  :: LinkHomologyTheory
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> (Labeling -> Set Labeling)
closureMap lht omt1 omt2 omt3 =
  diagonalMapSet (buildClosureHyperbox lht omt1 omt2 omt3)

buildClosureHyperbox
  :: LinkHomologyTheory
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> Hyperbox (Set Labeling) (Map Labeling (Set Labeling))
buildClosureHyperbox lht omt1 omt2 omt3 = buildHyperbox (getGroupAtDelta)
                                                        (uncurry buildArrows)
                                                        [d1, d2, d3]
 where
  forceCurry3 [x, y, z] = (x, y, z)
  d1    = length $ decos1 flatD
  d2    = length $ decos2 flatD
  d3    = length $ decos3 flatD
  flatD = flatCircularDiagram omt1 omt2 omt3
  getGroupAtDelta :: [Int] -> Set Labeling
  getGroupAtDelta =
    S.fromList . buildKhovanovGroup . getDiagramAtDelta . forceCurry3
  getDiagramAtDelta :: (Int, Int, Int) -> Configuration
  getDiagramAtDelta (i1, i2, i3)
    = let
        diag =
          unsafeCrossToFlat . closureD $ flatCircularDiagram omt1 omt2 omt3
        ps = take i1 (decos1 flatD) ++ take i2 (decos2 flatD) ++ take
          i3
          (decos3 flatD)
        config =
          buildConfig (S.fromList . componentsAsNodes $ diag) (S.fromList ps)
      in
        fromJust $ applyAllDecorations config
  -- getGroupAtDelta :: Map [Int] (Set Labeling)
  -- getGroupAtDelta = M.fromList
  --   [ ( [j1, j2, j3]
  --     , S.fromList $ buildKhovanovGroup (getDiagramAtDelta (j1, j2, j3))
  --     )
  --   | j1 <- [0 .. d1]
  --   , j2 <- [0 .. d2]
  --   , j3 <- [0 .. d3]
  --   ]

  buildArrows :: Delta -> Epsilon -> Map Labeling (Set Labeling)
  buildArrows d e = action . configurationMap lht $ buildArrows' d e
  closure      = flatCircularDiagram omt1 omt2 omt3
  allPlatDecos = [decos1 closure, decos2 closure, decos3 closure]
  buildArrows' :: Delta -> Epsilon -> Configuration
  buildArrows' d' e' =
    let epsDecos :: [Int] -> [Z2] -> [[Decoration]] -> [Decoration]
        epsDecos (_ : ds) (ZZero : es) (_ : decoss) = epsDecos ds es decoss
        epsDecos (d : ds) (ZOne : es) (deco : decoss) =
          if null (drop d $ deco) then []
          else (head . drop d $ deco) : epsDecos ds es decoss
        epsDecos [] _  _  = []
        epsDecos _  [] _  = []
        epsDecos _  _  [] = []
    in  buildConfig
          ( S.fromList
          . componentsAsNodes
          . unsafeCrossToFlat
          . closureD
          $ closure
          )
          (S.fromList $ epsDecos d' e' allPlatDecos)




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

data ClosureDiagram = ClosureDiagram {closureD :: CrossPD,
                                      decos1 :: [Decoration],
                                      decos2 :: [Decoration],
                                      decos3 :: [Decoration]} deriving (Eq, Ord, Show)

flatCircularDiagram
  :: OrientedMorseTangle
  -> OrientedMorseTangle
  -> OrientedMorseTangle
  -> ClosureDiagram
flatCircularDiagram omt1 omt2 omt3 = circularDiagram
  (omtToMt . orientedResolution $ omt1)
  (omtToMt . orientedResolution $ omt2)
  (omtToMt . orientedResolution $ omt3)

-- TODO: add CrossPlats!
-- TODO: add vertical strands at bottom for sake of orientations
-- remember that all diagrams are initially vertical!
-- all assumed to have the same strand number too
circularDiagram :: MorseTangle -> MorseTangle -> MorseTangle -> ClosureDiagram-- should be ClosureDiagram
circularDiagram t1 t2 t3 = ClosureDiagram fullDiagram d1 d2 d3
 where
  strands     = pointsOnBottom . mtangle $ t1
  t1bar       = mirrorMorse t1
  t2bar       = mirrorMorse t2
  t3bar       = mirrorMorse t3
  twoColumns  = leftColumn `sideBySide` rightColumn
  leftColumn' = disjointUnion
    [ mtangle t2bar
    , mtangle $ t2
    , fmap (fmap (shiftYBy (-1)))
    . mtangle
    $ t3bar -- disjoint union is not for gluing!  so need a shift
    , mtangle t3
    ]
  rightColumn' =
    disjointUnion [mirror . mtangle $ t1, mirror . mtangle $ t1bar]
  rBottom      = tangleBottom rightColumn'
  lBottom      = tangleBottom leftColumn'
  leftColumn   = extendBy (rBottom - lBottom) leftColumn'
  rightColumn  = extendBy (lBottom - rBottom) rightColumn'
  columnLength = tangleBottom leftColumn
  fullDiagram =
    (upsideDown $ markovPlats strands)
      ++ fmap (fmap (shiftYBy (strands - 1))) twoColumns
      ++ fmap (fmap (shiftYBy (strands + columnLength - 1)))
              (markovPlats strands)
  disjointUnion = foldl dUnionDiag []
  d `sideBySide` d' = d ++ fmap (fmap (shiftXBy strands)) d'
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
    fmap (fmap (shiftXBy strands . shiftYBy strands))
      . mplats
      $ (mirrorMorse t1)
  p1bar' = fmap (fmap (shiftYBy 1)) p1'
  p2bar' = fmap (fmap (shiftYBy strands)) . mplats $ (mirrorMorse t2)
  p2'    = fmap (fmap (shiftYBy 1)) p2bar'
  p3bar' =
    fmap (fmap (shiftYBy (2 * (tangleBottom (mtangle t2)) + strands + 1)))
      . mplats
      $ mirrorMorse t3
  p3' = fmap (fmap (shiftYBy 1)) p3bar'
  d1  = zipWith platDecorations p1bar' p1'
  d2  = zipWith platDecorations p2bar' p2'
  d3  = zipWith platDecorations p3bar' p3'

