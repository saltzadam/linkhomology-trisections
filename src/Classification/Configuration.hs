module Classification.Configuration
where
import Data.Set (Set)
import qualified Data.Set as S

import Core.Configuration
-- import Core.Configuration.Orientation
import Core.PlanarDiagram

insideDecs :: Configuration -> (Configuration -> Maybe PD) -> Maybe (Set Decoration)
insideDecs conf prim = do
                    primary <- prim conf
                    return $ S.filter (\d -> arrowJordan d primary == Inside) (decos conf)

insideDecs' :: Configuration -> PD -> Set Decoration
insideDecs' conf pd = S.filter (\d -> arrowJordan d pd == Inside) (decos conf)

outsideDecs :: Configuration -> (Configuration -> Maybe PD) -> Maybe (Set Decoration)
outsideDecs conf prim = do
                    primary <- prim conf
                    return $ S.filter (\d -> arrowJordan d primary == Outside) (decos conf)

outsideDecs' :: Configuration -> PD -> Set Decoration
outsideDecs' conf pd = S.filter (\d -> arrowJordan d pd == Outside) (decos conf)
