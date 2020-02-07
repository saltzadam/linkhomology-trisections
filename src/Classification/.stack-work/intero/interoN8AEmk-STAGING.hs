module Classification.NewE
-- goal is to make a new E classifier based on the idea that
-- E means "every pair of arrows gives a configuration of type 2 - 7"
where

import Classification.Configuration
import           Core.Configuration
import           Core.PlanarDiagram
import           Data.Set             (Set)
import qualified Data.Set             as S
import Data.Maybe (fromJust)
import Core.Util
import Core.Grid
import Classification.TypeE

data TwoDConf = TwoDConf { twoDactiveCircles :: Set Component,
                           twoDpassiveCircles :: Set Component,
                           twoDdecos :: (Decoration,Decoration)
                         } deriving (Eq, Ord, Show)

ruin :: TwoDConf -> TwoDConf
ruin (TwoDConf a p d) = TwoDConf a p (fmap reverseDeco d) -- remember that fmap only affects one.

confToTwoDConf :: Configuration -> Maybe TwoDConf
confToTwoDConf conf = if S.size (decos conf) == 2
                      then let [d1,d2] = S.toList (decos conf)
                           in Just $ TwoDConf (activeCircles conf) (passiveCircles conf) (d1,d2)
                      else Nothing

twoDConfToConf :: TwoDConf -> Configuration
twoDConfToConf (TwoDConf a p (d1,d2)) = Configuration a p (S.fromList [d1,d2])

buildTwoDConf :: Set Component -> (Decoration, Decoration) -> TwoDConf
buildTwoDConf cs (d1,d2) = fromJust (confToTwoDConf . buildConfig cs $ S.fromList [d1,d2])

twoDSubconfigurations :: Configuration -> [TwoDConf]
twoDSubconfigurations c = fmap removePassives configs where
  decoPairs = pairs (S.toList . decos $ c) :: [(Decoration,Decoration)]
  configs = buildTwoDConf (activeCircles c `S.union` passiveCircles c) <$> decoPairs :: [TwoDConf]
  removePassives :: TwoDConf -> TwoDConf
  removePassives (TwoDConf actives passives decos') = TwoDConf actives S.empty decos'

twoDE :: TwoDConf -> Bool
twoDE c = or (($ c) <$> [isType23, isType4, isType5, isType67away, isType67towards])

isType23 :: TwoDConf -> Bool
isType23 c = let c' = twoDConfToConf c
             in
               maybe False
                  (\x -> indegree x c' == 2 || outdegree x c' == 2)
                  (centralCircle c')
            && S.size (twoDactiveCircles c) == 3

isType4 :: TwoDConf -> Bool
isType4 c = let c' = twoDConfToConf c
            in
                  S.size (activeCircles c') == 1
              &&  ((S.size <$> insideDecs c' centralCircle) == Just 2
                  ||
                  (S.size <$> outsideDecs c' centralCircle) == Just 2)
              &&  let (d1,d2) = twoDdecos c
                  in maybe False (\x -> oppositeDecs x d1 d2) (centralCircle c')


isType5 :: TwoDConf -> Bool
isType5 c = let c' = orientConfigCCW' (twoDConfToConf c)
            in
                 S.size (activeCircles c') == 1
              && (S.size <$> insideDecs c' centralCircle) == Just 1
              &&  let (d1,d2) = twoDdecos c
                  in maybe False (sameDirection d1 d2) (centralCircle c')
              && all (isDualDegreeOne c') [d1,d2]

--away from centrla circle
-- combine with case
isType67away :: TwoDConf -> Bool
isType67away c = let c' = orientConfigCCW' (twoDConfToConf c)
                   in
                        S.size (activeCircles c') == 2
                     && S.size (selfDecs c' centralCircle) == 1 -- was if
                     && let [self] = S.toList $ selfDecs c' centralCircle -- was then
                            notSelf = other self (twoDdecos c)
                            in findNode c' (from notSelf) == centralCircle c'
                                && isDualDegreeOne c' self
                                && if (elem self <$> (S.toList <$> insideDecs c' centralCircle)) == Just True
                                   then decoOrientationRelPoint (fromJust $ centralCircle c') self (from notSelf) == Just CCW
                                   else decoOrientationRelPoint (fromJust $ centralCircle c') self (from notSelf) == Just CW


isType67towards :: TwoDConf -> Bool
isType67towards c = let c' = orientConfigCCW' (twoDConfToConf c)
                   in
                        S.size (activeCircles c') == 2
                     && S.size (selfDecs c' centralCircle) == 1 -- was if
                     && let [self] = S.toList $ selfDecs c' centralCircle -- was then
                            notSelf = other self (twoDdecos c)
                            in findNode c' (to notSelf) == centralCircle c'
                                && isDualDegreeOne c' self
                                && if (elem self <$> (S.toList <$> insideDecs c' centralCircle)) == Just True
                                   then decoOrientationRelPoint (fromJust $ centralCircle c') self (to notSelf) == Just CW
                                   else decoOrientationRelPoint (fromJust $ centralCircle c') self (to notSelf) == Just CCW




