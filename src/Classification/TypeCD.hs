{-|
Module      : Classification.TypeCD
Description : Helper functions for testing Type C and D configurations
Copyright   : (c) Adam Saltz, 2020
License     : GPL-3
Maintainer  : saltz.adam@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Classification.TypeCD
  (
  -- isC,
  -- getC,
    soleCircle
  ,
  -- isD,
  -- getD,
    oneDegree22Circle
  , decsAligned
  , inAndOutDegree1
  )
where
import           Core.Configuration
import           Core.PlanarDiagram

import           Algebra.Graph.AdjacencyMap
import           Control.Monad                  ( liftM2 )

-- The C, D, and E classifications revolve around some kind of main circle.
-- We call that circle primary.
-- type D primary
-- | Finds the sole in- and out-degree 2 component of a 'Configuration'.
oneDegree22Circle :: Configuration -> Maybe PD
oneDegree22Circle conf = if length counted == 1
  then Just . head $ counted
  else Nothing
 where
  counted = filter (\c -> indegreePD c conf == 2 && outdegreePD c conf == 2)
    $ vertexList (aGraph conf)

-- | Finds the sole active circle of a 'Configuration'.
-- type C primary
soleCircle :: Configuration -> Maybe PD
soleCircle c = if (vertexCount . aGraph $ c) == 1
  then Just (head . vertexList . aGraph $ c)
  else Nothing
-- hasPrimaryCircle p = isJust . p


decsAligned
  :: Configuration -> (Configuration -> Maybe PD) -> [Decoration] -> Maybe Bool
decsAligned conf prim (d : d' : ds) = liftM2
  (&&)
  (Just $ sameDirection (diagram conf) d d')
  (decsAligned conf prim (d' : ds))
decsAligned _ _ _ = Just True

-- --- D ---
inAndOutDegree1 :: Configuration -> Component -> Bool
inAndOutDegree1 conf c = indegree c conf == 1 && outdegree c conf == 1

