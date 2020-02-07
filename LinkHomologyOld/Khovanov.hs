module LinkHomology.Khovanov
(khovanovCobordismMap,
 kh
)
where
import Data.Maybe (catMaybes)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
 
import Core.Braids
import Core.Cube
import Core.Resolution
import Core.PlanarDiagram
import Core.Configuration
import Core.Configuration.Orientation
import Complex.BuildCube
import Algebra.V
import Algebra.Z2
import Core.Util
import Complex.Homology
import LinkHomology.LinkHomology

import Debug.Trace

kh :: LinkHomologyTheory
kh = LHT khovanovCobordismMap

khovanovCobordismMap :: Configuration -> DecorationMap
khovanovCobordismMap config
    | (S.size . decos $ config) /= 1         = DecorationMap M.empty
    | (S.size . activeCircles $ config) == 1 = split (head . S.toList . activeCircles $ config) (head . S.toList . decos $ config)
    | otherwise =  merge (listToTuple . S.toList . activeCircles $ config)  (head . S.toList . decos $ config)
    
alternatingSigns :: (Component,Component) -> [Labeling]
alternatingSigns (a,b) = [Labeling (M.fromList [(a,ZZero),(b,ZOne)]), Labeling (M.fromList [(a,ZOne),(b,ZZero)])]

split :: Component -> Decoration -> DecorationMap
split comp deco = let
                    sourceConfig = buildConfig (S.singleton comp) (S.singleton deco)
                    targetConfig = orientConfigCCW <$> decorationAction deco sourceConfig
                    plusSource = makeLabel  (const2 ZOne) sourceConfig
                    minusSource = makeLabel (const2 ZZero) sourceConfig
                    minusTarget = listToSetMod2 $ catMaybes [makeLabel (const2 ZZero) <$> targetConfig]
                    plusTargets' = let
                                      Just [a,b] = S.toList . components <$> targetConfig
                                   in
                                     listToSetMod2 $ alternatingSigns (a,b)
                 in 
                    DecorationMap . M.fromList $
                        [(minusSource, minusTarget), (plusSource, plusTargets')]
                        
merge :: (Component,Component) -> Decoration -> DecorationMap
merge (c1,c2) deco = let
                        sourceConfig = buildConfig (S.fromList [c1,c2]) (S.singleton deco)
                        targetConfig = orientConfigCCW' <$> decorationAction deco sourceConfig
                     in 
                        DecorationMap $
                            M.fromList . fmap (fmap S.fromList) . catMaybes . fmap (traverse sequence) $ 
                                       (makeLabel (const2 ZOne) sourceConfig
                                         , [makeLabel (const2 ZOne) <$> targetConfig])
                                       :
                                       fmap (\l -> (l,[makeLabel (const2 ZZero) <$> targetConfig])) (alternatingSigns (c1,c2))
                                       -- ,(makeLabel smallestPlus sourceConfig
                                       --   , [makeLabel (const2 ZZero) <$> targetConfig])
                                       -- ,(makeLabel biggestPlus sourceConfig
                                       --   , [makeLabel (const2 ZZero) <$> targetConfig])]


-- khPoincarePoly :: Braid -> String
-- khPoincarePoly br = cubePoincarePoly' hGrading qGrading 't' 'q' (khovanovCube . platClosure $ br)

-- khovanovRank :: CrossPD -> Int
-- khovanovRank = rank . homology . khovanovCube

