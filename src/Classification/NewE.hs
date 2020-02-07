module Classification.NewE where

import           Classification.Configuration
import           Core.Configuration
import           Core.PlanarDiagram
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.List
import           Core.Util
import           Core.Grid
import           Classification.TypeE
import           Control.Applicative
import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm
                                                ( reachable )

-- TODO: decide on "NewE" vs "E" etc.

data TwoDConf = TwoDConf { twoDactiveCircles :: PD,
                           twoDdecos :: (Decoration,Decoration),
                           twoDcomponents :: Set Component,
                           twoDaGraph :: AdjacencyMap PD
                         } deriving (Eq, Ord, Show)

twoDActiveComponents :: TwoDConf -> Int
twoDActiveComponents =
  S.size . vertexSet . activePart . twoDConfToConf 
-- TODO: move to testing
ruin :: TwoDConf -> TwoDConf
ruin (TwoDConf a d c g) = TwoDConf a (fmap reverseDeco d) c g -- remember that fmap only affects the second one.

-- TODO: is it correct that this doesn't look at p?

confToTwoDConf :: Configuration -> Maybe TwoDConf
confToTwoDConf conf@(Configuration _ ds c g) = if S.size ds == 2
  then
    let [d1, d2] = S.toList ds
    in  Just $ TwoDConf (activeCircles conf) (d1, d2) c g
  else Nothing

twoDConfToConf :: TwoDConf -> Configuration
twoDConfToConf (TwoDConf pd (d1, d2) c g) =
  Configuration pd (S.fromList [d1, d2]) c g


-- if initial configuration is oriented then so are the subconfigurations?  Seems like yes.
twoDSubconfigurations :: Configuration -> [TwoDConf]
twoDSubconfigurations (Configuration pd ds c g) = configs
 where
  decoPairs = pairs (S.toList ds) :: [(Decoration, Decoration)]
  configs =
        (\pair -> TwoDConf
           (activeCircles
             (Configuration pd (S.fromList [fst pair, snd pair]) c g)
           )
           pair
           c
           (activeGraph pd (S.fromList [fst pair, snd pair]))
         )
      <$> decoPairs :: [TwoDConf]
  -- removePassives :: TwoDConf -> TwoDConf
  -- removePassives (TwoDConf actives decos') = TwoDConf actives decos'

twoDE :: TwoDConf -> Bool
twoDE c =
  or (($ c) <$> [isType23, isType4, isType5, isType67away, isType67towards])

isType23 :: TwoDConf -> Bool
isType23 c =
  let pd       = twoDactiveCircles c
      (d1, d2) = twoDdecos c
  in  (  (arcInPD (to d1) pd && arcInPD (to d2) pd)
      || (arcInPD (from d1) pd && arcInPD (from d2) pd)
      )
      && twoDActiveComponents c
      == 3

isType4 :: TwoDConf -> Bool
isType4 c =
  let c' = twoDConfToConf c
  in  twoDActiveComponents c == 1
      && (  (S.size <$> insideDecs c' centralCircle)
         == Just 2
         || (S.size <$> outsideDecs c' centralCircle)
         == Just 2
         )
      && let (d1, d2) = twoDdecos c in oppositeDecs (diagram c') d1 d2


isType5 :: TwoDConf -> Bool
isType5 c =
  let c' = twoDConfToConf c
  in
    twoDActiveComponents c == 1
    && (S.size <$> insideDecs c' centralCircle)
    == Just 1
    && let (d1, d2) = twoDdecos c
       in  sameDirection (diagram c') d1 d2 && all (isDualDegreeOne c') [d1, d2]

--away from central circle
-- combine with case
isType67away :: TwoDConf -> Bool
isType67away c =
  let
    c' = twoDConfToConf c
    pd = diagram c'
  in
    twoDActiveComponents c
    == 2
    && S.size (selfDecs c' centralCircle)
    == 1 -- was if
    && let
         [self]                          = S.toList $ selfDecs c' centralCircle -- was then
         notSelf                         = other self (twoDdecos c)
         Decoration (Arc p  _) (Arc _ s) = self
         Decoration (Arc p' _) (Arc _ _) = notSelf
       in
         required
           (liftA2 elem
                   (findPoint c' (fst . arcToPair . from $ notSelf))
                   (pdComponents <$> centralCircle c')
           )
         && isDualDegreeOne c' self
         && if (elem self <$> (S.toList <$> insideDecs c' centralCircle))
               == Just True
            then
              if p' `elem` (p : fst (takeTil (reachable s (diagram c')) p))
                then
                  (getOrientation pd . head . vertexList <$> centralCircle c')
                    == Just CCW
                else
                  (getOrientation pd . head . vertexList <$> centralCircle c')
                    == Just CW
            else
              if p' `elem` (p : fst (takeTil (reachable s (diagram c')) p))
                then
                  (getOrientation pd . head . vertexList <$> centralCircle c')
                    == Just CW
                else
                  (getOrientation pd . head . vertexList <$> centralCircle c')
                    == Just CCW

isType67towards :: TwoDConf -> Bool
isType67towards c =
  let
    c' = twoDConfToConf c
    pd = diagram c'
  in
    twoDActiveComponents c
    == 2
    && S.size (selfDecs c' centralCircle)
    == 1 -- was if
    && let
         [self]                           = S.toList $ selfDecs c' centralCircle -- was then
         notSelf                          = other self (twoDdecos c)
         Decoration (Arc p _) (Arc _  s ) = self
         Decoration (Arc _ _) (Arc p' q') = notSelf
       in
         required
           (liftA2 elem
                   (findPoint c' (fst . arcToPair . to $ notSelf))
                   (pdComponents <$> centralCircle c')
           ) -- notSelf goes to big component
         && isDualDegreeOne c' self
         && if (elem self . S.toList <$> insideDecs c' centralCircle)
               == Just True
            then
              if not
                 $ null
                     (           [p', q']
                     `intersect` (p : fst
                                   (takeTil (reachable s (diagram c')) p)
                                 )
                     )
              then
                (getOrientation pd . head . vertexList <$> centralCircle c')
                  == Just CW
              else
                (getOrientation pd . head . vertexList <$> centralCircle c')
                  == Just CCW
            else
              if not
                 $ null
                     (           [p', q']
                     `intersect` (p : fst
                                   (takeTil (reachable s (diagram c')) p)
                                 )
                     )
              then
                (getOrientation pd . head . vertexList <$> centralCircle c')
                  == Just CCW
              else
                (getOrientation pd . head . vertexList <$> centralCircle c')
                  == Just CW
