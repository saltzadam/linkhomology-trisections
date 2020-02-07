module Classification.TypeE where

import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Maybe                     ( listToMaybe
                                                , isNothing
                                                )
import           Data.List                      ( nub
                                                , (\\)
                                                )
import           Data.Foldable                  ( find )
import           Control.Arrow                  ( (***) )
import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm
                                                ( reachable )

import           Core.Util                      (takeTil)
import           Core.PlanarDiagram     
import           Core.Configuration
-- import Core.Configuration.Orientation
import           Core.Grid
-- import Core.Util
import           Classification.Configuration


degreeOneDecs :: Configuration -> Set Decoration
degreeOneDecs c = decos c `S.difference` selfDecs c centralCircle

isDualDegreeOne :: Configuration -> Decoration -> Bool
isDualDegreeOne conf (Decoration (Arc p q) (Arc r s)) =
  let pd      = diagram conf
      (interval1, interval2) =
        (S.fromList *** S.fromList) . makeOK $ takeTil (reachable p pd) q  -- (p...,q...)
      makeOK (ls, ls') = (head ls' : ls, tail ls')
      decoPoints =
        ( S.unions
          . S.toList
          . S.map (\((p', q'), (r', s')) -> S.fromList [p', q', r', s'])
          . S.map (\d -> (arcToPair . to $ d, arcToPair . from $ d))
          $ decos conf
          )
          `S.difference` S.fromList [p, q, r, s]
  in  null (interval1 `S.intersection` decoPoints)
        || null (interval2 `S.intersection` decoPoints)

-- TODO: combine these by paremetrizing over Jordan

nextInsideIsOrientedCorrectly :: Configuration -> Decoration -> Bool
nextInsideIsOrientedCorrectly conf dec =
  isDualDegreeOne conf dec && nextNode `maybeElem` fmap arcToPair froms
 where
  pd           = diagram conf
  comp' = head . filter (arcInPD (to dec)) $ (vertexList . aGraph $ conf)
  nextInterval = snd (intervals pd dec)
  tos          = fmap to . S.toList $ insideDecs' conf comp'
  froms        = fmap from . S.toList $ insideDecs' conf comp'
  nextNode =
    find (`elem` edgeList nextInterval) (fmap arcToPair (tos ++ froms))
  maybeElem :: Eq a => Maybe a -> [a] -> Bool
  maybeElem Nothing  _  = False
  maybeElem (Just x) xs = x `elem` xs

nextOutsideIsOrientedCorrectly :: Configuration -> Decoration -> Bool
nextOutsideIsOrientedCorrectly conf dec =
  isDualDegreeOne conf dec && nextNode `maybeElem` fmap arcToPair froms
 where
    --dualDegreeOnes :: Configuration -> Component ->  [Decoration]
    --dualDegreeOnes con (Comp com) = filter (\d -> from d `elem` com)  (S.toList . decos $ con)
  pd           = diagram conf
  --comp' = head . filter (arcInPD (to dec)) $ (vertexList . aGraph $ conf)
  nextInterval = snd (intervals pd dec)
  tos          = fmap to . S.toList $ outsideDecs' conf pd
  froms        = fmap from . S.toList $ outsideDecs' conf pd
  nextNode =
    find (`elem` edgeList nextInterval) (fmap arcToPair (tos ++ froms))
  maybeElem :: Eq a => Maybe a -> [a] -> Bool
  maybeElem Nothing  _  = False
  maybeElem (Just x) xs = x `elem` xs


-- type E primary
centralCircle :: Configuration -> Maybe PD
centralCircle conf =
  let ends =
        fmap (\d -> (fst . arcToPair $ to d, fst . arcToPair $ from d))
          . S.toList
          $ decos conf
      candidates =
        filter (\pd -> containsAtLeastOneOfEach (vertexSet pd) ends)
               (vertexList $ aGraph conf) :: [PD]
  in  if length candidates == 1 then listToMaybe candidates else Nothing

containsAtLeastOne :: Ord a => Set a -> (a, a) -> Bool
containsAtLeastOne big (x, y) = x `S.member` big || y `S.member` big
containsAtLeastOneOfEach :: Ord a => Set a -> [(a, a)] -> Bool
containsAtLeastOneOfEach big = all (containsAtLeastOne big)


selfDecs :: Configuration -> (Configuration -> Maybe PD) -> Set Decoration
selfDecs conf f = case f conf of
  Nothing -> S.empty
  Just pd -> S.filter
    (\(Decoration (Arc p q) (Arc p' q')) ->
      all (`S.member` vertexSet pd) [p, q, p', q']
    )
    (decos conf)

-- TODO: functions Confiuration -> Maybe PD should be f, not p

insideSelfDecs
  :: Configuration -> (Configuration -> Maybe PD) -> Maybe (Set Decoration)
insideSelfDecs conf p = S.intersection (selfDecs conf p) <$> insideDecs conf p

outsideSelfDecs
  :: Configuration -> (Configuration -> Maybe PD) -> Maybe (Set Decoration)
outsideSelfDecs conf p =
  S.intersection (selfDecs conf p) <$> outsideDecs conf p

notSelfDecs :: Configuration -> (Configuration -> Maybe PD) -> Set Decoration
notSelfDecs conf p = decos conf `S.difference` selfDecs conf p

-- TODO: parametrize over Jordan

allInsideOppositeDirection
  :: Configuration -> (Configuration -> Maybe PD) -> Maybe Bool
allInsideOppositeDirection conf p
  | isNothing (p conf) = Nothing
  | isNothing oneD = Just True
  | otherwise = do
    isds <- insideSelfDecs conf p
    one  <- oneD
    return $ and (oppositeDecs (diagram conf) one <$> (S.toList isds \\ [one]))
  where oneD = listToMaybe . S.toList =<< insideSelfDecs conf p

allOutsideOppositeDirection
  :: Configuration -> (Configuration -> Maybe PD) -> Maybe Bool
allOutsideOppositeDirection conf p
  | isNothing (p conf) = Nothing
  | isNothing oneD = Just True
  | otherwise = do
    isds <- outsideSelfDecs conf p
    one  <- oneD
    return $ and (oppositeDecs (diagram conf) one <$> (S.toList isds \\ [one]))
 where
  oneD :: Maybe Decoration
  oneD = listToMaybe . S.toList =<< outsideSelfDecs conf p

compareOutsideAndInsideDirections
  :: Configuration -> (Configuration -> Maybe PD) -> Maybe Bool
compareOutsideAndInsideDirections conf p
  | isNothing primary = Nothing
  | isNothing oneInside || isNothing oneOutside = Just True
  | otherwise = sameDirection (diagram conf) <$> oneInside <*> oneOutside
 where
  primary    = p conf
  oneInside  = listToMaybe . S.toList =<< insideSelfDecs conf p
  oneOutside = listToMaybe . S.toList =<< outsideSelfDecs conf p

allDegreeOneCirclesTowardsP
  :: Configuration -> (Configuration -> Maybe PD) -> Maybe Bool
allDegreeOneCirclesTowardsP conf p = fmap not (allDegreeOneCirclesAwayP conf p)

allDegreeOneCirclesAwayP
  :: Configuration -> (Configuration -> Maybe PD) -> Maybe Bool
allDegreeOneCirclesAwayP conf p
  | isNothing (p conf) = Nothing
  | S.null (notSelfDecs conf p) = Just True
  | otherwise = do
    pd <- p conf
    return $ all (flip arcInPD pd . from) (S.toList $ notSelfDecs conf p)

composeOrientations :: [Orientation] -> Maybe Orientation
composeOrientations cs = case nub cs of
  [CW ] -> Just CW
  [CCW] -> Just CCW
  _     -> Nothing
