{-# LANGUAGE MultiParamTypeClasses #-}
module LinkHomology.LinkHomology
  ( qGrading
  , qGradingSets
  , hGrading
  , LinkHomologyTheory(..)
  , chainComplex'
  , chainComplex
  , orientedLabeling
  , ConfigurationMap
  , LinearMap
  , fullConfMap
  , nullMap
  , allLabels
  )
where
import           Data.Map.Lazy                  ( Map )
import           Data.Set                       ( Set )
import qualified Data.Map.Lazy                 as M
import qualified Data.Set                      as S

import           Algebra.V
import           Core.Util
import           Core.Resolution
import           Algebra.Z2
import           Core.Configuration
-- import           Core.Configuration.Orientation
import           Core.PlanarDiagram
import           Core.Cube
import           Complex.BuildCube
import           Core.Grid
import           Control.Monad                  ( replicateM )
-- ZOne = A
-- ZZero = B

-- TODO: double-check ring instances with mod2

type ConfigurationMap = Configuration -> (Labeling -> Set Labeling)
type LinearMap = Labeling -> Set Labeling

--right place to add coefficients
newtype LinkHomologyTheory = LHT {
                              configurationMap :: ConfigurationMap
                              }

fullConfMap :: LinkHomologyTheory -> ConfigurationMap
fullConfMap = extend . configurationMap


chainComplex
  :: LinkHomologyTheory -> CrossPD -> LinearCubeOf Resolution Labeling
chainComplex lht = instantiate . chainComplex' lht

-- instantiateAndStupify
--   :: CubeOf Resolution Labeling LinearMap
--   -> LinearCubeOf Resolution DumbLabeling
-- instantiateAndStupify =
--   LinearCubeOf
--     . fmap (fmap (fmap (S.map stupify)) . M.mapKeys stupify)
--     . arrows
--     . instantiate

-- should this just be CubeOf Resolution Configuration LinearMap?

chainComplex'
  :: LinkHomologyTheory -> CrossPD -> CubeOf Resolution Labeling LinearMap
chainComplex' lht pd =
  let
    go' :: Ord o => Map [o] j -> Map o j
    go' = M.fromList . fmap swap . concatMap sequence . fmap swap . M.toList
  in
    CubeOf
    . fmap
        ( fmap
            (fmap
              ( fullConfMap lht )
            )
        . go'
        . M.mapKeys (buildKhovanovGroup . drdToConfig)
        )
    . arrows'
    . cubeOfConfigurations
    $ pd


nullMap :: p -> Set a
nullMap _ = S.empty


hGrading :: (Resolution, o) -> Int
hGrading (r, _) = weight r

qGrading :: (Resolution, Labeling) -> Int
qGrading (r, x) = qGrading' x + hGrading (r, x)
 where
  qGrading' :: Labeling -> Int
  qGrading' = sum . fmap (decToInt . snd) . M.toList . getLabeling
  decToInt :: Z2 -> Int
  decToInt ZOne  = 1
  decToInt ZZero = -1

qGradingSets :: (Resolution, Set Labeling) -> Int
qGradingSets (r, ls) = minimum . S.map (curry qGrading r) $ ls



-- assumes oriented
orientedLabeling :: PD -> Labeling
orientedLabeling pd =
  let rasmussenLevel comp = nestingLevel pd comp + orientationNumber (head $ S.toList comp)
      orientationNumber pt = if getOrientation pd pt == CCW then 0 else 1
  in  Labeling
      . fmap (\r -> if odd r then ZOne else ZZero)
      . graphMap rasmussenLevel
      . pdComponents
      $ pd
      
allLabels :: Configuration -> S.Set Labeling
allLabels conf = S.fromList
  [ Labeling . M.fromList $ zip (S.toList . components $ conf) ls
  | ls <- replicateM (S.size . components $ conf) [ZZero, ZOne]
  ]

extendMapPassiveRule
  :: Set Component -> (Labeling -> Set Labeling) -> Labeling -> Set Labeling
extendMapPassiveRule comps f (Labeling l) =
  if comps `S.isSubsetOf` (S.fromList . M.keys $ l)
    then
      let activeL  = M.restrictKeys l comps
          passiveL = l `M.difference` activeL
      in  S.map (mappend (Labeling passiveL)) $ f (Labeling activeL)
    else S.empty

extend :: ConfigurationMap -> ConfigurationMap
extend confF c = extendMapPassiveRule (S.fromList . pdComponents . activeCircles $ c) (confF c) 
