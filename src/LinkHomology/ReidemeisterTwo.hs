{-# LANGUAGE MultiWayIf #-}
module LinkHomology.ReidemeisterTwo where

import           Core.Configuration      hiding ( Vertical
                                                , Horizontal
                                                )
import           Core.PlanarDiagram
import           Core.Grid


import qualified Data.Set                      as S
import Algebra.Graph.AdjacencyMap (vertexSet)
import Algebra.Graph.AdjacencyMap.Algorithm (scc)
import Data.Maybe (catMaybes, fromJust)

r2topology :: CrossDeco' Point -> PD -> Maybe Component
r2topology cd pd = case determineR2Type pd cd of
  Braidlike -> Nothing
  AntiBraidlike ->
    let ((Arc p _), (Arc p' _)) =
          snd $ uncross cd (determineOrientation cd)
        candidate = searchPD (\pt -> pt == p || pt == p') pd
        
    in  if (S.size $ vertexSet (scc candidate)) == 1
        then Just $ vertexSet candidate
        else Nothing

r2HandleAttachment :: CrossDeco' Point -> PD -> Maybe PD
r2HandleAttachment cd pd = case determineR2Type pd cd of
  Braidlike -> Just pd
  AntiBraidlike ->  do
    comp <- r2topology cd pd
    let pd' = deleteCompPD pd comp
    let dec = fst $ uncross cd (determineOrientation cd)
    pdResult <- longOneHandleAttachment dec pd'
    return pdResult

attachCrossDecCorrectly :: (CrossDeco, PD) -> (Maybe Decoration, PD)
attachCrossDecCorrectly (cd, pd) = (maybeDec, newPD)
 where 
  maybeDec = attachCrossDec cd pd
  newPD    = if maybeDec == Nothing
    then pd
    else
    let Just dec = maybeDec
    in  fromJust . longOneHandleAttachment dec . diagram $ deleteComps
            (buildConfig (S.singleton pd) S.empty)
            (catMaybes [r2topology cd pd])
foldCDCS :: PD -> [CrossDeco] -> [Decoration]
foldCDCS _ []    = []
foldCDCS pd [cdc] = catMaybes [fst $ attachCrossDecCorrectly (cdc, pd)]
foldCDCS pd cdcs  = catMaybes . reverse $ go pd cdcs []
 where
  go _ [] list = list
  go pd' (cdc : cdcss) list =
    let result = attachCrossDecCorrectly (cdc, pd')
    in  go (snd result) cdcss (fst result : list)

        -- candidates =
        --   filter (\c -> arcInComp arc1 c && arcInComp arc2 c) (pdComponents pd)
        -- c = if length candidates == 1
        --   then head candidates
        --      -- FIX: this is extremely dumb, just make the candidate the one with the other two nodes!!!
        --   else
        --     error
        --     $  "R2 decoration intersects "
        --     ++ show (length candidates)
        --     ++ " "
        --     ++ show cd
        --     ++ " "
        --     ++ show pd
