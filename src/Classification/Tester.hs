{-|
Module      : Classification.Tester
Description : Tests for configuration types.
Copyright   : (c) Adam Saltz, 2020
License     : GPL-3
Maintainer  : saltz.adam@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Classification.Tester where
--import Classification.Classification
--import Classification.TypeAB
import           Core.Configuration
-- import           Core.Configuration.Orientation
import           Core.Grid
import           Core.PlanarDiagram
import           Core.Util

import           Classification.TypeCD

import           Classification.Configuration
import           Classification.NewE

import qualified Data.Map as M
import qualified Data.Set                      as S
import           Data.Maybe                     ( catMaybes
                                                , listToMaybe
                                                )
import           Data.List                      (nub)
import           Data.Semigroup
import           Algebra.Graph.AdjacencyMap

-- | Builds a configuration from a 'String'.  There are some examples in the folder @Classification/Testing/Test.hs@.

-- | Determining if a 'Configuration' has a certain type takes several steps.
-- Each of these steps is a 'Test'.
data Test = Test {test :: Configuration -> Bool,
                  name :: String}

-- | Compose two tests with @&&@.  The names are composed by concatenating the names with a carriage return.
instance Semigroup Test where
  (<>) t t' = Test (\c -> test t c && test t' c) (cleanTrailingReturn (name t ++ "\n" ++ name t') )

-- | The "identity test" is @const True@ and has no name.
instance Monoid Test where
  mempty = Test {test = const True,
                 name = ""}
  mappend = (<>)

-- | Applys a list of tests to a 'Configuration' and prints the results.
allTestIO :: [Test] -> Configuration -> IO ()
ts `allTestIO` conf = putStrLn . unlines . fmap (`tests` conf) $ ts


-- | @test `tests` configuration@ returns @(name test): ✓@ if @configuration@ satisfies the @test@ and @(name test): x@ otherwise.
tests :: Test -> Configuration -> String
t `tests` conf =
  name t ++ ": " ++ (if test t conf then checkmark else notcheckmark)

-- * The tests
-- * Type A
-- | Test for two active circles.
testA1 :: Test
testA1 =
  Test {test = (2 ==) . countActiveCircles, name = "Two active circles"}

-- | Test for one target circle.
testA2 :: Test
testA2 = Test
  { test = \c ->
    ( length
      . nub
      . catMaybes
      . fmap (findPoint c . (\(Arc p _) -> p) . to)
      $ S.toList (decos c)
      )
      == 1
  , name = "One target circle"
  }

-- | Test for one passive circle.  The set @[testA1, testA2, testA3]@ is redundant.
testA3 :: Test
testA3 = Test
  { test = \c ->
    ( length
      . nub
      . catMaybes
      . fmap (findPoint c . (\(Arc p _) -> p) . from)
      $ S.toList (decos c)
      )
      == 1
  , name = "One source circle"
  }
-- * Type B

-- | Test that every component has outdegree 1.
testB1 :: Test
testB1 = Test
  { test = \c ->
    and [ outdegree com c == 1 | com <- pdComponents (activeCircles c) ]
  , name = "All outdegrees are 1"
  }

-- | Test that every component has indegree 1.
testB2 :: Test
testB2 = Test
  { test = \c -> and [ indegree com c == 1 | com <- pdComponents ( activeCircles c) ]
  , name = "All indegrees are 1"
  }

-- * Type C
-- | Test for one active circle.
testC1 :: Test
testC1 =
  Test {test = (== 1) . countActiveCircles, name = "One active circle"}

-- | Test that there are decorations inside the active circle.
testC2 :: Test
testC2 = Test
  { test = \c ->
    let p = soleCircle in required $ not . S.null <$> insideDecs c p
  , name = "There are inside decorations (or can't find primary circle):"
  }

-- | Test that there are decorations outside the active circle.
testC3 :: Test
testC3 = Test
  { test = \c ->
    let p = soleCircle in required $ not . S.null <$> insideDecs c p
  , name = "There are outside decorations (or can't find primary circle):"
  }

-- | Test that all the inside decorations face the same way.
testC4 :: Test
testC4 = Test
  { test = \c ->
    let p = soleCircle
    in  required $ decsAligned c p =<< (S.toList <$> insideDecs c p)
  , name = "Inside decorations all point the same way"
  }

-- | Test that all the outside decorations face the same way.
testC5 :: Test
testC5 = Test
  { test = \c ->
    let p = soleCircle
    in  required $ decsAligned c p =<< (S.toList <$> outsideDecs c p)
  , name = "Outside decorations all point the same way"
  }

-- | Test that the inside and outside decorations are oriented corrctly.  'testC4' and 'testC5' guarantee that we only need to verify this for one pair of decorations.
testC6 :: Test
testC6 = Test
  { test = \c ->
    let p = soleCircle
    in  required $ do
          inside     <- insideDecs c p
          oneInside  <- listToMaybe . S.toList $ inside
          outside    <- outsideDecs c p
          oneOutside <- listToMaybe . S.toList $ outside
          return $ betterInterlaced c oneInside oneOutside
  , name = "Inside and outside decorations interlaced"
  }

-- | Test that the inside and outside decorations are "interlaced," i.e. if you walk around the active circle, you see the tip/tail of alternating decorations.
testC7 :: Test
testC7 = Test
  { test = \c ->
    let p = soleCircle
    in
      required
        (do
          inside     <- insideDecs c p
          outside    <- outsideDecs c p
          oneInside  <- listToMaybe . S.toList $ inside
          oneOutside <- listToMaybe . S.toList $ outside
          let (i, i') =  intervals (diagram c) (Decoration (from oneInside) (to oneInside))
          let nearbyOutsiders = S.fromList [fst . arcToPair . from $ oneOutside, snd . arcToPair . from $ oneOutside, fst . arcToPair . to $ oneOutside, snd . arcToPair . to $ oneOutside]
          return
            ((S.size (vertexSet i `S.intersection` nearbyOutsiders)
             == 1
             )
            && (  S.size
                   (vertexSet i' `S.intersection` nearbyOutsiders)
               == 1
               )
            )
        )
  , name = "inside separates outside"
  }



-- first interval starts with node', second with node
-- deco node node'
-- betterInterlaced :: Configuration -> Decoration -> Decoration -> Bool
-- betterInterlaced conf (Decoration (Arc p q) (Arc p' q')) (Decoration (Arc r s) (Arc r' s')) =
--   (\(c1, _) -> (fst . arcToPair . to $ outside) `S.member` (vertexSet c1))
--     (intervals (diagram conf) inside)
-- want
--  --------
--  |  ___n'|
-- m|>|m'_^
--  |     n|
--  -------
--  so should see m n m' n'

-- | Compares the orientations from 'testC6'.  (It's better than the old @interlaced@.)  (TODO: change some names -- this is not the same sense of interlaced as 'testC7'!!)
betterInterlaced :: Configuration -> Decoration -> Decoration -> Bool
betterInterlaced conf (Decoration (Arc p q) (Arc p' q')) (Decoration (Arc r s) (Arc r' s')) =
  let list1 = [p,q,r,s,p',q',r',s']
      list2 = [p,q,r',s',p',q',r,s]
  in reachables list1 (diagram conf) || reachables list2 (diagram conf)


-- * Type D
-- TODO: why are there 7 tests for C and 5 for D?
-- | Test that all the active circles except one have in- and out-degree 1.
testD1 :: Test
testD1 = Test
  { test = \c ->
    let actives = pdComponents . activeCircles $ c -- use indegreePD etc?
    in  (length . filter (inAndOutDegree1 c) $ actives)
          == (length actives - 1)
  , name = "All but one actives are in- and out-degree 1"
  }

-- | Test that the circle with in- and out-degree 2 has decorations inside it.
testD2 :: Test
testD2 = Test
  { test = let p = oneDegree22Circle
           in  \c -> required $ not . S.null <$> insideDecs c p
  , name = "Has inside decorations"
  }

-- | Test that the circle with in- and out-degree 2 has decorations outside it.
testD3 :: Test
testD3 = Test
  { test = let p = oneDegree22Circle
           in  \c -> required $ not . S.null <$> outsideDecs c p
  , name = "Has outside decorations"
  }


-- | Test that the decorations around the big circle are properly aligned.
testD4 :: Test
testD4 = Test
  { test = let p = oneDegree22Circle
           in
             \c -> required
               (do
                 central <- p c
                 let toCentral =
                       uncurry Decoration .
                       listToTuple .
                       fmap to
                         . filter (\d -> to d `arcInPD` central)
                         $ (S.toList . decos $ c)
                 let interval1 = fst $ intervals central toCentral
                 let fromCentral = 
                       uncurry Decoration
                       . listToTuple
                       . fmap to
                       . filter (\d -> from d `arcInPD` central)
                         $ (S.toList . decos $ c)
                 return
                   (length (filter (`arcInPD` interval1) [to fromCentral, from fromCentral]) == 2 || not
                     (any (`arcInPD` interval1) [to fromCentral, from fromCentral])
                   )
               )
  , name = "Decorations on big circle are properly aligned"
  }

-- symmetrize :: (Ord a) => S.Set (Node a) -> S.Set (Node a)
-- symmetrize ns = ns `S.union` S.map flipNode ns


-- | Test that the inside and outside decorations are "interlaced," i.e. if you walk around the active circle, you see the tip/tail of alternating decorations.
testD5 :: Test
testD5 = Test
  { test = let p = oneDegree22Circle
           in
             \c -> required
               (do
                 central  <- p c
                 insiders <- insideDecs c p
                 let fromCentral =
                       S.filter (\d -> from  d `arcInPD` central) (decos c)
                 let fromCInside =
                       head . S.toList $ insiders `S.intersection` fromCentral
                 let fromCOutside =
                       head . S.toList $ S.delete fromCInside fromCentral
                 let toCentral = S.filter (\d -> to  d `arcInPD` central) (decos c)
                 let toCInside =
                       head . S.toList $ insiders `S.intersection` toCentral
                 let toCOutside =
                       head . S.toList $ S.delete toCInside toCentral
                 let nearbyOutsiders =
                       S.map (fst . arcToPair) $ S.fromList [from fromCOutside, to toCOutside]
                 let (i, i') = intervals (diagram c ) (Decoration (from fromCInside) (to toCInside))
                 return
                   (  (  S.size
                          (                vertexSet i
                          `S.intersection` nearbyOutsiders
                          )
                      == 1
                      )
                   && (  S.size
                          (                vertexSet i'
                          `S.intersection` nearbyOutsiders
                          )
                      == 1
                      )
                   )
               )
  , name = " inside separates outside"
  }
testD6 :: Test
testD6 = Test
  { test = let p = oneDegree22Circle
           in
             \c -> required
               (do
                 primary <- p c
                 inside  <- insideDecs c p
                 outside <- outsideDecs c p
                 let toCentral = S.filter (\d -> to d `arcInPD` primary) (decos c)
                 let fromCentral =
                       S.filter (\d -> from  d `arcInPD` primary) (decos c)
                 insideFrom <-
                   listToMaybe
                   . S.toList
                   $ (fromCentral `S.intersection` inside)

                 outsideFrom <-
                   listToMaybe
                   . S.toList
                   $ (fromCentral `S.intersection` outside)

                 insideTo <-
                   listToMaybe . S.toList $ (toCentral `S.intersection` inside)
                 outsideTo <-
                   listToMaybe . S.toList $ (toCentral `S.intersection` outside)
                 let newD1 = Decoration (from insideFrom) (to insideTo)
                 let newD2 = Decoration (from outsideFrom) (to outsideTo)
                 return $ betterInterlaced c newD1 newD2
               )
  , name = "interlaced"
  }
-- * Complete tests
-- | Now we compose the various tests.  E.g. @testA = mconcat [testA1, testA2, testA3]@.
testA :: Test
testA = mconcat [testA1, testA2, testA3] -- testA4
testB :: Test
testB = Test {test = \c -> let theGraph = activePart c
                           in ((S.size . vertexSet . activePart $ c) > 1) &&  (all (== 1)) (M.elems . fmap S.size . adjacencyMap $ theGraph),
               name = "graph criterion for B"}

testC :: Test
testC = mconcat [testC1, testC2, testC3, testC4, testC5, testC6, testC7]
testD :: Test
testD = mconcat [testD1, testD2, testD3, testD4, testD5, testD6]
-- TODO: take another look at D tests

-- | The component tests for type E has its own module, 'Classification.NewE'. 
testE :: Test
testE = Test
  { test = all twoDE . twoDSubconfigurations
  , name = "every twod config is of type 2 - 7"
  }

-- | Types A and B are dual.  So are types C and D.  So you could write @testC = testDual typeD@.  Unfortunately computing dual configurations is quite slow, so this isn't practical.
testDual :: Test -> Test
testDual t = t { test = maybe False (test t) . dualConfiguration
               , name = "dual: " ++ name t
               }

-- ** Test and print
-- | @testIsA configuration@ shows the output of each individual 'Test' which makes up 'testIsA'.  Very useful for testing.
testIsA :: Configuration -> IO ()
testIsA = allTestIO [testA1, testA2, testA3] -- testA4
testIsB :: Configuration -> IO ()
testIsB = allTestIO [testB]
testIsC :: Configuration -> IO ()
testIsC = allTestIO [testC1, testC2, testC3, testC4, testC5, testC6]
testIsD :: Configuration -> IO ()
testIsD = allTestIO [testD1, testD2, testD3, testD4, testD5]
testIsE :: Configuration -> IO ()
--testIsE = allTestIO [testE1, testE2, testE3, testE4, testE5]
testIsE = allTestIO [testE]

-- * Pretty printing functions.
fromPic :: String -> Configuration
fromPic =
  orientConfig
    . drdToConfig
    . gridPicToDRD
    . buildGridPic
    . lines

checkmark :: String
checkmark = "✓"
notcheckmark :: String
notcheckmark = "x"

cleanTrailingReturn :: String -> String
cleanTrailingReturn x = case last x of
  '\n' -> init x
  '\r' -> init x
  _    -> x


