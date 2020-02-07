module LinkHomology.SSS where

import           LinkHomology.BarNatan
import           LinkHomology.Szabo
import           LinkHomology.LinkHomology
import           Core.Configuration
import           Algebra.V
import Core.CubeUtil

import           Data.Set                       ( Set )

-- don't need kh in here because it's included in bn

sssOneHandleMap :: Configuration -> Labeling -> Set Labeling
sssOneHandleMap conf l = bnOneHandleMap conf l `addSetsMod2` szaboOneHandleMap conf l

sss :: LinkHomologyTheory
sss = LHT sssOneHandleMap

