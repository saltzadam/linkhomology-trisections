{-# LANGUAGE MultiWayIf #-}
module Classification.Classification

where
import Data.Set (size, Set)
import qualified Data.Set as S

import Core.Configuration

import Classification.TypeCD
import Classification.TypeE
import Classification.Configuration
import Classification.Tester

import Core.Util

data Classifier = Classifier {classifyAs :: Configuration -> Bool,
                              parameter  :: Configuration -> [Int]}

buildClassifierFromTests :: (Configuration -> [Int]) -> [Test] -> Classifier
buildClassifierFromTests f ts = Classifier (\c -> and (($ c) . test <$> ts)) f

classifyIsA :: Classifier
classifyIsA = buildClassifierFromTests (\c -> [size . decos $ c]) [testA1, testA2, testA3] --testA4

classifyIsB :: Classifier
-- classifyIsB = buildClassifierFromTests (\c -> [size . decos $ c]) [testB1, testB2]
classifyIsB = buildClassifierFromTests (\c -> [size . decos $ c]) [testB]

classifyIsC :: Classifier
classifyIsC = buildClassifierFromTests (\c -> [maybeToInt . fmap size . insideDecs c $ soleCircle , maybeToInt . fmap size . outsideDecs c $ soleCircle]) [testC1, testC2, testC3, testC4, testC5, testC6,testC7]

classifyIsD :: Classifier
classifyIsD =   buildClassifierFromTests (\c -> [maybeToInt . fmap size . insideDecs c $ oneDegree22Circle , maybeToInt . fmap size . outsideDecs c $ oneDegree22Circle]) [testD]

classifyIsE :: Classifier
classifyIsE = buildClassifierFromTests (\c -> [size . degreeOneDecs $ c , size . selfDecs c $ centralCircle])  [testE]


-- TODO: wonder if there's a way to order tests to make this fastest.  seems like E must be the slowest
classifyConfiguration :: Configuration -> Set SzabosConfigs
classifyConfiguration config = if | size (decos config) == 0 -> S.singleton PassiveConfig
                                  | size (decos config) == 1 -> S.singleton KhovanovConfig
                                  | disconnected config      -> S.singleton Disconnected
--                                  | size (decos config) == 2 -> --S.unions classifyingSets -- because only dim 2 can have multiple classifications
                                  | classifyAs classifyIsA config -> S.singleton $ A  (head . parameter classifyIsA $ config)
                                  | classifyAs classifyIsB config -> S.singleton $ B  (head . parameter classifyIsB $ config)
                                  | classifyAs classifyIsC config -> if size (decos config) == 2
                                    then cset `S.union` dset
                                    else
                                      S.singleton $ uncurry C  (listToTuple . parameter classifyIsC $ config)
                                  | classifyAs classifyIsD config -> S.singleton $ uncurry D  (listToTuple . parameter classifyIsD $ config)
                                  | classifyAs classifyIsE config -> S.singleton $ uncurry E  (listToTuple . parameter classifyIsE $ config)
                                  | otherwise -> S.singleton NoneOfTheAbove
                               where
                                 buildSet :: Bool -> a -> S.Set a
                                 buildSet True x' = S.singleton x'
                                 buildSet False _ = S.empty
                                 -- classifyingSets = [aset,bset,cset,dset,eset]
                                 -- aset = buildSet (classifyAs classifyIsA config) (A (head . parameter classifyIsA $ config))
                                 -- bset = buildSet (classifyAs classifyIsB config) (B (head . parameter classifyIsB $ config))
                                 cset = buildSet (classifyAs classifyIsC config) ( uncurry C (listToTuple . parameter classifyIsC $ config))
                                 dset =  buildSet (classifyAs classifyIsD config) ( uncurry D (listToTuple . parameter classifyIsD $ config))
                                 -- eset = buildSet (classifyAs classifyIsE config) ( uncurry E (listToTuple . parameter classifyIsE $ config))
