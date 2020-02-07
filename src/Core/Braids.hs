{-# LANGUAGE FlexibleInstances, DataKinds #-}
module Core.Braids where
import           Data.List.Split                ( splitOneOf )
import           Data.Char                      ( isNumber )
import           Data.Maybe                     ( catMaybes )

import           Core.PlanarDiagram
import           Core.Grid


-- | A 'Braid' is a width and word.
-- Integers in the word represent Artin generators and their invereses.  E.g. @[1,3,-2]@ represents the word \sigma_1\sigma_3\sigma_2^{-1}.
data Braid = Braid { braidWidth :: Int
                   , braidWord :: [Int]}
                   deriving (Eq, Show, Read)

bridgeNumber :: Braid -> Int
bridgeNumber b = braidWidth b `div` 2

-- | Parse command line input into a braid.
parse :: [String] -> (Braid, Int)
parse input = (braid, mark)
 where
  braid = Braid {braidWord = parseWord word, braidWidth = width}
  parseWord =
    map (read :: String -> Int) . filter (not . null) . splitOneOf ",]["
  (mark, word, width) = if all isNumber (head input)
    then (read $ head input :: Int, input !! 1, read $ input !! 2 :: Int)
    else (1, head input, read $ input !! 1 :: Int)

-- | Parse a 'Braid' into a 'PD'.
markovClosure :: Braid -> CrossPD
markovClosure braid =
  fmap
      (shiftNodeYBy (width - 1))
      (concat
        [ crossingToNodes2 c width (2 * d - 1) | (c, d) <- zip word [1 ..] ]
      )
    ++ fmap (shiftNodeYBy (width - 1))                 (strands (2 * len) width)
    ++ fmap (shiftNodeYBy (2 * length word + width)) (markovPlats width)
    ++ upsideDown (markovPlats width)
 where
  word  = braidWord braid
  width = braidWidth braid

  len   = length . braidWord $ braid

strands :: Int -> Int -> CrossPD
strands lengt width =
  [ CrossJoin (Point (width + i) (0 + j)) (Point (width + i) (1 + j))
  | i <- [0 .. width - 1]
  , j <- [1 .. lengt]
  ]

markovPlats :: Int -> CrossPD
markovPlats 0 = []
markovPlats i =
  fmap (shiftNodeXBy 1) (markovPlats (i - 1))
    ++ [ CrossJoin (Point 0 j) (Point 0 (j + 1)) | j <- [0 .. i - 1] ]
    ++ [ CrossJoin (Point j i) (Point (j + 1) i) | j <- [0 .. 2 * (i - 1)] ]
    ++ [ CrossJoin (Point (2 * (i - 1) + 1) (j + 1)) (Point (2 * (i - 1) + 1) j)
       | j <- [0 .. i - 1]
       ]

platTop :: Int -> CrossPD
platTop width =
  let plat =
        [ CrossJoin (Point 0 0) (Point 0 1)
        , CrossJoin (Point 0 0) (Point 1 0)
        , CrossJoin (Point 1 0) (Point 1 1)
        ]
  in  concat [ shiftNodeXBy (2 * i) <$> plat | i <- [0 .. (bridgeNumber' - 1)] ]
  where bridgeNumber' = width `div` 2

platBottom :: Int -> Int -> CrossPD
platBottom width length' =
  let plat =
        [ CrossJoin (Point 0 0) (Point 0 1)
        , CrossJoin (Point 0 1) (Point 1 1)
        , CrossJoin (Point 1 1) (Point 1 0)
        ]
  in  shiftNodeYBy (length' + 1) <$> concat
        [ shiftNodeXBy (2 * i) <$> plat | i <- [0 .. (bridgeNumber' - 1)] ]
  where bridgeNumber' = width `div` 2

-- markovCube :: Braid -> CubeOf Resolution DecoratedResolvedDiagram
-- markovCube b = cubeOfResolutionsDecorated . markovClosure $ b 

mirror :: Braid -> Braid
mirror b = Braid {braidWidth = braidWidth b, braidWord = mirror' (braidWord b)}
 where
  mirror' :: [Int] -> [Int]
  mirror' = fmap (* (-1)) . reverse

-- TODO: why are there two of these lol

crossingToNodes :: Int -> Int -> Int -> [CrossNode Point]
crossingToNodes crossing width level =
  catMaybes $ toNode <$> [0 .. (width - 1)]
 where
  crossX = abs crossing
  toNode :: Int -> Maybe (CrossNode Point)
  toNode k | k == crossX - 1 && crossing < 0 = Just $ Cross
    Neg
    (Point k level)
    (Point (k + 1) level)
    (Point k (level + 1))
    (Point (k + 1) (level + 1))
  toNode k | k == crossX - 1 && crossing > 0 = Just $ Cross
    Pos
    (Point k level)
    (Point (k + 1) level)
    (Point k (level + 1))
    (Point (k + 1) (level + 1))
  toNode k | k == crossX = Nothing
  toNode k = Just $ CrossJoin (Point k level) (Point k (level + 1))

crossingToNodes2 :: Int -> Int -> Int -> [CrossNode Point]
crossingToNodes2 crossing width level =
  catMaybes (toNode <$> [0 .. (width - 1)])
    ++ [ CrossJoin (Point i (level + 1)) (Point i (level + 2))
       | i <- [0 .. (width - 1)]
       ]
 where
  crossX = abs crossing
  toNode :: Int -> Maybe (CrossNode Point)
  toNode k | k == crossX - 1 && crossing < 0 = Just $ Cross
    Neg
    (Point k level)
    (Point (k + 1) level)
    (Point k (level + 1))
    (Point (k + 1) (level + 1))
  toNode k | k == crossX - 1 && crossing > 0 = Just $ Cross
    Pos
    (Point k level)
    (Point (k + 1) level)
    (Point k (level + 1))
    (Point (k + 1) (level + 1))
  toNode k | k == crossX = Nothing
  toNode k = Just $ CrossJoin (Point k level) (Point k (level + 1))

