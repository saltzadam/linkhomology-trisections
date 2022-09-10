{-|
Module      : Clsasification.Configuration
Description : Some helper functions to classify configurations
Copyright   : (c) Adam Saltz, 2020
License     : GPL-3
Maintainer  : saltz.adam@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Classification.Configuration
where
import Data.Set (Set)
import qualified Data.Set as S

import Core.Configuration
-- import Core.Configuration.Orientation
import Core.PlanarDiagram

-- | Returns the set of 'Decoration' which lie inside some component of a 'Configuration'.  The second argument picks out the component.  For example, it could pick out the component which is the target of a particular 'Decoration'.  But that component may not be found, so you get a 'Maybe'.
insideDecs :: Configuration -> (Configuration -> Maybe PD) -> Maybe (Set Decoration)
insideDecs conf prim = do
                    primary <- prim conf
                    return $ S.filter (\d -> arrowJordan d primary == Inside) (decos conf)

-- | If we already know the name of the component, use this. (TODO: rewrite as @insideDecs' c pd = insideDecs c (const (Just pd))@)

insideDecs' :: Configuration -> PD -> Set Decoration
insideDecs' conf pd = S.filter (\d -> arrowJordan d pd == Inside) (decos conf)

outsideDecs :: Configuration -> (Configuration -> Maybe PD) -> Maybe (Set Decoration)
outsideDecs conf prim = do
                    primary <- prim conf
                    return $ S.filter (\d -> arrowJordan d primary == Outside) (decos conf)

outsideDecs' :: Configuration -> PD -> Set Decoration
outsideDecs' conf pd = S.filter (\d -> arrowJordan d pd == Outside) (decos conf)
