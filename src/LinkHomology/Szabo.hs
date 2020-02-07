{-# LANGUAGE MultiWayIf #-}
module LinkHomology.Szabo
  ( sz
  , szaboOneHandleMap
  )
where
import           Core.Configuration
-- import           Core.Configuration.Orientation
import           Algebra.V
import           Algebra.Z2
import           Core.Util
import           Core.CubeUtil
import           Classification.Classification
import           LinkHomology.LinkHomology
import           LinkHomology.BarNatan
import           Classification.TypeE
import           Core.PlanarDiagram

import           Data.Maybe                     ( fromJust )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import qualified Data.Map.Lazy                 as M


sz :: LinkHomologyTheory
sz = LHT totalSzMap

totalSzMap :: Configuration -> Labeling -> Set Labeling
totalSzMap conf l =
  szaboOneHandleMap conf l `addSetsMod2` khOneHandleMap conf l


szaboOneHandleMap :: ConfigurationMap
szaboOneHandleMap conf
  | classifyConfiguration conf == S.singleton PassiveConfig = rzero
  | classifyConfiguration conf == S.singleton KhovanovConfig = rzero
  | -- khovanovCobordismMap conf
    classifyConfiguration conf == S.singleton NoneOfTheAbove = rzero
  | classifyConfiguration conf == S.singleton Disconnected = rzero
  | otherwise = \l ->
    unionsMod2 . fmap (($ l) . aux) . S.toList $ classifyConfiguration conf
 where
  aux (A _  )        = typeAmap conf
  aux (B _  )        = typeBmap conf
  aux (C _ _)        = typeCmap conf
  aux (D _ _)        = typeDmap conf
  --aux (E _ _) = DecorationMap M.empty
  aux (E _ _)        = typeEmap conf
  aux Disconnected   = error "aux applied to disconnected"
  aux KhovanovConfig = error "aux applied to Khovanov"
  aux PassiveConfig  = error "aux applied to PassiveConfig"
  aux NoneOfTheAbove = error "aux applied to NoneOfTheAbove"

typeAmap :: ConfigurationMap
typeAmap conf l =
  let sourceLabel =
        makeLabel (const ZOne) (pdComponents . diagram $ activeConf conf)
      targetConfig = fromJust . applyAllDecorations . activeConf $ conf -- ALL labels
  in  if l == sourceLabel then allLabels targetConfig else S.empty


typeBmap :: ConfigurationMap
typeBmap conf =
  let
  -- sourceLabel = makeLabel (const2 ZZero) (activePart conf) -- ALL labels
      targetLabel =
        fromJust
          . fmap (makeLabel (const ZZero) . pdComponents . diagram)
          $ applyAllDecorations (activeConf conf)
  in  const $ listToSetMod2 [targetLabel]

--whoa!
typeCmap :: ConfigurationMap
typeCmap = typeAmap

--whoa!!
typeDmap :: ConfigurationMap
typeDmap = typeBmap

typeEmap :: ConfigurationMap
typeEmap conf (Labeling l) =
  let targetConf = fromJust . dualConfiguration $ activeConf conf
  in  case centralCircle conf of
        Just comp -> if l M.! head (pdComponents comp) == ZOne
          then case centralCircle targetConf of
            Just comp' -> S.filter
              (\(Labeling l') -> l' M.! head (pdComponents comp') == ZZero)
              (allLabels targetConf)
            Nothing -> S.empty
          else S.empty
        Nothing -> S.empty

