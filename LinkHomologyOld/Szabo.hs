{-# LANGUAGE MultiWayIf #-}
module LinkHomology.Szabo
  (sz, szaboOneHandleMap)
where
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
import Classification.Classification
import LinkHomology.LinkHomology
import LinkHomology.Khovanov
import Classification.TypeE

import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M


sz :: LinkHomologyTheory
sz = LHT szaboOneHandleMap

szaboOneHandleMap :: Configuration -> DecorationMap
szaboOneHandleMap conf
  | classifyConfiguration conf == S.singleton PassiveConfig = DecorationMap M.empty
  | classifyConfiguration conf == S.singleton KhovanovConfig = khovanovCobordismMap conf
  | classifyConfiguration conf ==S.singleton NoneOfTheAbove = DecorationMap M.empty
  | classifyConfiguration conf ==S.singleton Disconnected = DecorationMap M.empty
  |  otherwise = mconcat . fmap aux . S.toList $ classifyConfiguration conf
                             where
                             aux (A _) = typeAmap conf
                             aux (B _) = typeBmap conf
                             aux (C _ _) = typeCmap conf
                             aux (D _ _) = typeDmap conf
                             --aux (E _ _) = DecorationMap M.empty
                             aux (E _ _) = typeEmap conf
                             aux Disconnected = error "aux applied to disconnected"
                             aux KhovanovConfig = error "aux applied to Khovanov"
                             aux PassiveConfig = error "aux applied to PassiveConfig"
                             aux NoneOfTheAbove = error "aux applied to NoneOfTheAbove"


typeAmap :: Configuration -> DecorationMap
typeAmap conf =  let
                  sourceLabel = makeLabel (const2 ZOne) (activePart conf)
                  targetLabel = fromJust . fmap (makeLabel (const2 ZOne)) $ (applyAllDecorations . activePart $ conf)
                 in
                  DecorationMap . M.fromList $
                    [(sourceLabel, listToSetMod2 [targetLabel])]
  
typeBmap :: Configuration -> DecorationMap
typeBmap conf = let
                  sourceLabel = makeLabel (const2 ZZero) (activePart conf)
                  targetLabel = fromJust . fmap (makeLabel (const2 ZZero)) $ (applyAllDecorations . activePart $ conf)
                in
                  DecorationMap . M.fromList $
                    [(sourceLabel, listToSetMod2 [targetLabel])]

--whoa!
typeCmap :: Configuration -> DecorationMap
typeCmap = typeAmap

--whoa!!
typeDmap :: Configuration -> DecorationMap
typeDmap = typeBmap

typeEmap :: Configuration -> DecorationMap
typeEmap conf = let
                  plusOnCentral :: Configuration -> Component -> Z2
                  plusOnCentral conf' comp = if Just comp == centralCircle conf' then ZOne else ZZero
                  minusOnCentral conf' comp = if Just comp == centralCircle conf' then ZZero else ZOne
                  sourceLabel = makeLabel plusOnCentral (activePart conf)
                  --targetLabel = makeLabel minusOnCentral (fromJust . applyAllDecorations . activePart $ conf)
                  targetLabel = makeLabel minusOnCentral . orientConfigCCW . fromJust . dualConfiguration . activePart $ conf
                in
             DecorationMap . M.fromList $
               [(sourceLabel, listToSetMod2 [targetLabel])]

activePart :: Configuration -> Configuration
activePart conf = conf {passiveCircles = S.empty}

-- szaboRank :: CrossPD -> Int
-- szaboRank = rank . homology . szaboCube
