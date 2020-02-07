module LinkHomology.SSS where

import           LinkHomology.Khovanov
import           LinkHomology.BarNatan
import           LinkHomology.Szabo
import           LinkHomology.LinkHomology
import           Core.Configuration
import           Algebra.V
import           Core.Resolution
import           Core.Cube
import           Complex.BuildCube
import           Core.PlanarDiagram
import           Core.Util

import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Map.Lazy ( Map )
import qualified Data.Map.Lazy as M

sssOneHandleMap :: Configuration -> DecorationMap
sssOneHandleMap conf =
  mconcat [barNatanOneHandleMap conf, szaboOneHandleMap conf]

sss :: LinkHomologyTheory
sss = LHT sssOneHandleMap

-- bnCube
--   :: CrossPD -> CubeOf Resolution (Set Labeling) (Map Labeling (Set Labeling))
-- bnCube pd = cube4
--  where
--   cube2 = cubeOfConfigurations pd
--   cube3 =
--     onMors (barNatanOneHandleMap . orientConfigCCW)
--       . onObjs (S.fromList . buildKhovanovGroup . drdToConfig)
--       $ cube2
--   cube4 = cube3
--     { arrows = M.mapWithKey
--                  (\r map' ->
--                    fmap (\dm -> applyToSet dm (vertices cube3 M.! r)) map'
--                  )
--                  (arrows cube3)
--     }
-- --  cube5 = cube4 {arrows = fmap (fmap (fmap S.fromList)) (arrows cube4)}
--   applyToSet dm ls =
--     M.fromListWith S.union . graphF (applyAction dm) $ S.toList ls




-- sssCube
--   :: CrossPD -> CubeOf Resolution (Set Labeling) (Map Labeling (Set Labeling))
-- sssCube pd = cube4
--  where
--   cube2 = cubeOfConfigurations pd
--   cube3 =
--     onMors (sssOneHandleMap . orientConfigCCW)
--       . onObjs (S.fromList . buildKhovanovGroup . drdToConfig)
--       $ cube2
--   cube4 = cube3
--     { arrows = M.mapWithKey
--                  (\r map' ->
--                    fmap (\dm -> applyToSet dm (vertices cube3 M.! r)) map'
--                  )
--                  (arrows cube3)
--     }
-- --  cube5 = cube4 {arrows = fmap (fmap (fmap S.fromList)) (arrows cube4)}
--   applyToSet dm ls =
--     M.fromListWith S.union . graphF (applyAction dm) $ S.toList ls


