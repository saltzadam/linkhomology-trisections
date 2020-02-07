module LinkHomology.Multiplication where

import           LinkHomology.LinkHomology
import           LinkHomology.Cobordism
import           Core.Tangle
import           Core.PlanarDiagram
import           Core.Configuration
import           Core.Grid
import           Core.Cube
import           Algebra.V
import           Algebra.Z2

import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Map.Lazy ( Map )
import qualified Data.Map.Lazy as M
import           Data.List                      ( (\\)
                                                , minimumBy
                                                , maximumBy
                                                , sortBy
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.Ord                       ( comparing )
import           Data.Semigroup
import           Data.List.NonEmpty             ( fromList )

type BigLabeling = Set Labeling

data MultiplicationDiagram = MD {t1t2 :: TwoSidedLink,
                                 t2t3 :: TwoSidedLink} deriving (Eq, Show)


-- TODO: at some point should be sure to sort plats
shiftTSL :: Int -> TwoSidedLink -> TwoSidedLink
shiftTSL j (TSLink diag lp rp) = TSLink (shift diag) (shift lp) (shift rp)
  where shift = fmap (fmap (shiftYBy j))



makeMultiplicationDiagram
  :: MorseTangle -> MorseTangle -> MorseTangle -> MultiplicationDiagram
makeMultiplicationDiagram mt1 mt2 mt3 = MD
  (mt1 `glueMorseTangle` mt2)
  (shift (mt2 `glueMorseTangle` mt3))
 where
  shift = shiftTSL (1 + tangleBottom (diagramTSL (mt1 `glueMorseTangle` mt2)))

mdDecos :: MultiplicationDiagram -> [Decoration]
mdDecos md = zipWith Decoration
                     (unsafeCrossToFlat . rightPlats . t1t2 $ md)
                     (unsafeCrossToFlat . leftPlats . t2t3 $ md)

-- TODO: this assumes that diagrams are plat closures of braids!

multiply' :: LinkHomologyTheory -> MultiplicationDiagram -> MapOfLinearCubes
multiply' lht md = if null decs
  then error "can't multiply without plats!"
  else sconcat . fromList $ zipWith attach decs diags
 where
  diag0  = diagramTSL (t1t2 md) <> diagramTSL (t2t3 md)
  decs   = mdDecos md
  diags  = scanl (flip decorationActionCrossPD) diag0 decs :: [CrossPD]
  attach = oneHandleAttachment lht :: Decoration -> CrossPD -> MapOfLinearCubes

multiply
  :: LinkHomologyTheory
  -> MorseTangle
  -> MorseTangle
  -> MorseTangle
  -> MapOfLinearCubes
multiply lht mt1 mt2 mt3 =
  multiply' lht (makeMultiplicationDiagram mt1 mt2 mt3)

multiplyLabelsFlat
  :: LinkHomologyTheory
  -> MorseTangle
  -> MorseTangle
  -> MorseTangle
  -> [Z2]
  -> [Z2]
  -> Maybe [[Z2]]
multiplyLabelsFlat lht mt1 mt2 mt3 s1 s2 =
  let map' = multiply lht mt1 mt2 mt3 :: MapOfLinearCubes
      l1 :: Labeling
      l1 = makeLabel'
        (s1 ++ s2)
        ( fromJust
        . crossToFlat
        . diagramTSL
        $ dUnionTSL (glueMorseTangle mt1 mt2) (glueMorseTangle mt2 mt3)
        )
      makeLabel' :: [Z2] -> PD -> Labeling
      makeLabel' ss pd = Labeling $ M.fromList $ zip (componentsAsNodes pd) ss
  in  fmap (S.toList . S.map (M.elems . getLabeling)) . applyMapFlatUnsafe map' $ l1 

-- this assumes that the plat is horizontal, as it should be
-- also components must be oriented
attachFatOneHandle :: Decoration -> Configuration -> Maybe Configuration
attachFatOneHandle de@(Decoration (Join a b) (Join c d)) config =
  case findDecoration config de of
    Nothing -> Nothing -- if you can't find de in config, stop
    Just _  -> Just config' -- if you can, return config'
 where
  theComp      = fromJust $ findNode config (Join a b)
  oppositeComp = fromJust $ findNode config (Join c d)
  toTheLeft :: Component -> Node Point -> [Node Point]
  toTheLeft (Comp comp) (Join a b) =
    let c = shiftXBy (-1) a
        d = shiftYBy (-1) b
    in  if Join c d `elem` comp
          then Join c d : toTheLeft (Comp comp) (Join c d)
          else []
  toTheRight :: Component -> Node Point -> [Node Point]
  toTheRight (Comp comp) (Join a b) =
    let c = shiftXBy 1 a
        d = shiftXBy 1 b
    in  if Join c d `elem` comp
          then Join c d : toTheRight (Comp comp) (Join c d)
          else []
  fullPlat = reverse (toTheLeft theComp (Join a b)) ++ [Join a b] ++ toTheRight
    theComp
    (Join a b)
  oppositePlat =
    let distance' = _y b - _y c in fmap (fmap (shiftYBy distance')) fullPlat
  a' = minimumBy (comparing _x) (getPointsPD fullPlat)
  b' = maximumBy (comparing _x) (getPointsPD fullPlat)
  c' = maximumBy (comparing _x) (getPointsPD oppositePlat)
  d' = minimumBy (comparing _x) (getPointsPD oppositePlat)
  -- a' b'
  -- d' c'
  -- following is much simpler than usual because we know the form is the above
  -- TODO: I think the other decorationAction function 
  -- doesn't know that components can be oriented!
  stringBetween :: Point -> Point -> [Node Point]
  stringBetween above below = go above below []
   where
    go :: Point -> Point -> [Node Point] -> [Node Point]
    go a' b' ns = if a' == b'
      then ns
      else go (shiftYBy 1 a') b' (Join a' (shiftYBy 1 a') : ns)
  deleteNodes :: Component -> [Node Point] -> Component
  deleteNodes (Comp c) ns = Comp (c \\ ns)
  newComponents =
    S.fromList
      . componentsAsNodes'
      . S.toList -- put everything back together
      . S.insert (Comp (stringBetween a' c' ++ stringBetween b' d'))
      . S.map (`deleteNodes` (fullPlat ++ oppositePlat))
      $ components config -- take the circles in the configuration 
  config' = buildConfig newComponents decos'
  decos'  = S.delete de (decos config)

---- order the handles
-- right now ordering only works for plat form or hybrid form
-- should only be used on decorations between facing tangles

-- note that the ordering doesn't depend on the resolution
platOrdering :: Decoration -> Decoration -> Ordering
platOrdering (Decoration (Join p _) _) (Decoration (Join q _) _) =
  compare (_x p) (_x q)



platToDecorations :: CrossNode Point -> CrossNode Point -> Decoration
platToDecorations (CrossJoin p p') (CrossJoin q q') =
  Decoration (Join p p') (Join q q')
