{-|
Module      : Core.Draw
Description : ASCII pictures
Copyright   : (c) Adam Saltz, 2020
License     : GPL-3
Maintainer  : saltz.adam@gmail.com
Stability   : experimental
Portability : POSIX

Draw ASCII pictures of drawable things.
-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Core.Draw
  ( Draw(..)
  , superImpose
  , drawIO
  , dconcat
  )
where
import           Core.Grid
import           Core.PlanarDiagram
import           Core.Configuration
import qualified Data.Set                      as S
import qualified Data.Map.Lazy               as M
import           Algebra.V
import           Algebra.Z2
import LinkHomology.Closure3
import           Core.Tangle

import Algebra.Graph.AdjacencyMap

-- | Like @Show@ but for pictures rather than strings.
class Draw a where
  draw :: a -> String

-- | @drawIO = putStrLn . draw@
drawIO :: Draw a => a -> IO ()
drawIO = putStrLn . draw

--as on top of bs
superImposeLine :: String -> String -> String
superImposeLine (a:as) (b:bs) = if a == ' ' then b : superImposeLine as bs
                                else a : superImposeLine as bs
superImposeLine [] bs = bs
superImposeLine as [] = as
  

-- TODO: deduplication
-- | Superimpose one string on top of another.  E.g. @superimpose "that" "this string" = "that string"
superImpose :: String -> String -> String
superImpose as bs = let
  asl = lines as
  bsl = lines bs
  in
    if length asl >= length bsl
    then unlines $ zipWith superImposeLine asl bsl ++ (asl `cutOff` bsl)
    else unlines $ zipWith superImposeLine asl bsl ++ (bsl `cutOff` asl)
    where
        cutOff :: [a] -> [a] -> [a]
        xs `cutOff` ys = drop (length ys) xs

                      
  
-- Superimpose a collection of drawings.   Superimposition priority goes left to right.
dconcat :: Draw a => [a] -> String
dconcat = superImposeFold draw

superImposeFold :: (a -> String) -> [a] -> String
superImposeFold f (x : xs) = foldl (flip (superImpose . f)) (f x) xs
superImposeFold _ []       = []

-- instance Draw CrossPD where
--   draw cpd = superImposeFold draw $ cpd

-- instance Draw PD where
--   draw = draw . flatToCross

instance Draw TwoSidedLink where
  draw = draw . diagramTSL

instance Draw MorseTangle where
  draw (MT t _) = draw t

instance Draw CrossPD where
  draw = show . mconcat . fmap gridIt

instance Draw Point where
  draw p = show $ GridPic [p] [] []

instance Draw (Point,Point) where
  draw (p,q) = show $ GridPic [p,q] [(p,q)] []

instance Draw PD where
  draw =  superImposeFold draw . edgeList


gridIt :: CrossNode Point -> GridPic
gridIt (CrossJoin p q) = GridPic [p, q] [(p, q)] []
gridIt (Cross Pos a b c d) =
  GridPic [a, b, c, d] [(a, c), (b, d)] [((a, c), (b, d))]
gridIt (Cross Neg a b c d) =
  GridPic [a, b, c, d] [(a, b), (c, d)] [((a, b), (c, d))]

instance Draw (CrossNode Point) where
  draw = show . gridIt

instance Draw Decoration where
  draw  (Decoration (Arc p p') (Arc q q')) = show $ GridPic [] [] [((p,p'),(q,q'))]

instance Draw Configuration where
  draw config = (draw . diagram $ config)
                `superImpose`
                dconcat (S.toList . decos $ config)

instance Draw Closure3Diagram where
  draw (Closure3Diagram d d1 d2 d3 _ _ _) = draw d `superImpose`  dconcat (d1 ++ d2 ++ d3) -- draw d <> mconcat  (fmap draw d1) <> mconcat (fmap draw d2) <> mconcat (fmap draw d3)

drawLabel :: Component -> Z2 -> String
drawLabel c ZOne  =  -- replaceChar '|' 'a' . replaceChar '-' 'a' .
  replaceChar '.' 'a' (superImposeFold draw . S.toList $ c) 
drawLabel c ZZero =  -- replaceChar '|' 'b' . replaceChar '-' 'b' .
  replaceChar '.' 'b' (superImposeFold draw . S.toList $ c)

replaceChar :: Char -> Char -> String -> String
replaceChar a b = fmap (\x -> if x == a then b else x)

instance Draw (Component,Z2) where
  draw = uncurry drawLabel

instance Draw Labeling where
  draw = dconcat . M.toList . getLabeling

-- instance Draw DecorationMap where
--   draw =  unlines . concat . fmap (uncurry (:)) . M.toList .  M.mapKeys draw . fmap (fmap draw . S.toList) . action
-- drawPD' :: CrossPD -> GridPic
-- drawPD' = foldMap drawNode

-- drawFlat :: PD -> String
-- drawFlat = draw . flatToCross

-- draw :: CrossPD -> String
-- draw = show . drawPD'

-- draw' :: CrossPD -> IO ()
-- draw' = putStrLn . draw

-- drawNode :: CrossNode Point -> GridPic
-- drawNode (CrossJoin p q) = GridPic [p,q] [(p,q)] []
-- drawNode (Cross Pos a b c d) = GridPic [a, b, c, d] [(a,c), (b,d)] [((a,c),(b,d))]
-- drawNode (Cross Neg a b c d) = GridPic [a, b, c, d] [(a,b), (c,d)] [((a,b),(c,d))]

-- drawDeco :: Decoration -> GridPic
-- drawDeco (Decoration (Join p p') (Join q q')) = GridPic [] [] [((p,p'),(q,q'))]

-- drawConfig :: Configuration -> String
-- drawConfig config = show $
--                       (drawPD' . flatToCross . getPD $ config)
--                       `mappend`
--                       mconcat (fmap drawDeco (S.toList . decos $ config))

-- drawConfig' :: Configuration -> IO ()
-- drawConfig' = putStrLn . drawConfig

-- drawLabel :: Component -> Z2 -> String
-- drawLabel (Comp c) ZOne =  "\34" ++  drawFlat c ++ "\0"
-- drawLabel (Comp c) ZZero =  "\31" ++  drawFlat c ++ "\0"

-- drawLabeling :: Labeling -> String
-- drawLabeling l =  concatMap (uncurry drawLabel) . M.toList $ (getLabeling l)

-- drawMap :: DecorationMap -> [[String]]
-- drawMap dec = fmap (uncurry (:)) . M.toList .  mapKeys drawLabeling . fmap (fmap drawLabeling . S.toList) $ (action dec) 

-- drawMap' :: DecorationMap -> IO ()
-- drawMap' = putStrLn . unlines . fmap unlines . drawMap  
