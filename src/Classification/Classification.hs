{-|
Module      : Classification.Classification
Description : Classification of 'Configuration'
Copyright   : (c) Adam Saltz, 2020
License     : GPL-3
Maintainer  : saltz.adam@gmail.com
Stability   : experimental
Portability : POSIX

Classifies diagrams according to [Szabo](https://arxiv.org/abs/1010.4252) and [Sarkar-Seed-Szbo](https://arxiv.org/abs/1410.2877).
-}
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

-- | 'Classifier' is like a wrapper for 'Configuration -> (Bool, [Int])'.
-- @classifyAs@ asks whether a 'Configuration' has a certain type.  
-- @parameter@ is a measure of complexity of that type.  For example,
-- a 'Configuration' could have type A 4 or C 2 3.
--
-- @parameter@ returns a list but in practice it is always either
-- a single 'Int' or a pair of 'Int's.

data Classifier = Classifier {classifyAs :: Configuration -> Bool,
                              parameter  :: Configuration -> [Int]}

-- | Builds a 'Classifier'.  The first argument is simply the 'parameter'.  The second is a collection of 'Test's, which are defined in 'Classification.Tester'.
buildClassifierFromTests :: (Configuration -> [Int]) -> [Test] -> Classifier
buildClassifierFromTests f ts = Classifier (\c -> and (($ c) . test <$> ts)) f

-- | 'Classifier' for type A 'Configuration's.
classifyIsA :: Classifier
classifyIsA = buildClassifierFromTests (\c -> [size . decos $ c]) [testA1, testA2, testA3] --testA4

-- | 'Classifier' for type B 'Configuration's.
classifyIsB :: Classifier
-- classifyIsB = buildClassifierFromTests (\c -> [size . decos $ c]) [testB1, testB2]
classifyIsB = buildClassifierFromTests (\c -> [size . decos $ c]) [testB]

-- | 'Classifier' for type C 'Configuration's.
classifyIsC :: Classifier
classifyIsC = buildClassifierFromTests (\c -> [maybeToInt . fmap size . insideDecs c $ soleCircle , maybeToInt . fmap size . outsideDecs c $ soleCircle]) [testC1, testC2, testC3, testC4, testC5, testC6,testC7]

-- | 'Classifier' for type D 'Configuration's.
classifyIsD :: Classifier
classifyIsD =   buildClassifierFromTests (\c -> [maybeToInt . fmap size . insideDecs c $ oneDegree22Circle , maybeToInt . fmap size . outsideDecs c $ oneDegree22Circle]) [testD]

-- | 'Classifier' for type E 'Configuration's.
classifyIsE :: Classifier
classifyIsE = buildClassifierFromTests (\c -> [size . degreeOneDecs $ c , size . selfDecs c $ centralCircle])  [testE]

-- TODO: wonder if there's a way to order tests to make this fastest.  seems like E must be the slowest
-- | Classifies a 'Configuration' according to Szabo's scheme.  A 'Configuration' can have more than one type.
--
-- To work with the Sarkar-Seed-Szabo link we also need to classify trees and 
-- dual trees.  So this function isn't the final word on classification.
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
