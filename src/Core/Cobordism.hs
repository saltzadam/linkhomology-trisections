module Core.Cobordism
where
import Data.List
import Data.Maybe
import qualified Data.Set as S

import Core.PlanarDiagram
import Core.Util
import Core.Grid


data BasicCobordism' a = Merge a a a | Split a a a deriving (Eq, Show)
type BasicCobordism = BasicCobordism' Component

-- decoToCobordism :: Decoration -> DecoratedResolvedDiagram -> Maybe BasicCobordism
-- decoToCobordism deco drd = case findDecoration deco drd of
--     [Comp c,Comp c'] -> Just $ Merge (Comp c) (Comp c') (Comp (c ++ c'))
--     [c]    -> Just $ Split c (firstComp c) (secondComp c)
--     []     -> Nothing
--     _      -> error "your decoration is weird"
--     where
--         firstComp c = head $ splitComponent c deco
--         secondComp c = head . tail $ splitComponent c deco
--         splitComponent :: Component -> Decoration -> [Component]
--         splitComponent (Comp comp) (Decoration (Join a b) (Join c d)) = 
--               componentsAsNodes
--             . replaceBy (Join a b) (Join a d) 
--             . replaceBy (Join c d) (Join b c)
--             $ comp

findDecoration :: Decoration -> DecoratedResolvedDiagram -> [Component]
findDecoration (Decoration n n') drd = catMaybes [findNode n drd, findNode n' drd]

-- TODO: why is this here

findNode :: Node Point -> DecoratedResolvedDiagram -> Maybe Component
findNode n drd = find (\x -> n `S.member` x ) $ nodeComponents drd

