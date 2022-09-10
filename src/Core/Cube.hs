{-|
Module      : Core.Cube
Description : Cubes of resolutions and cubical chain complexes
Copyright   : (c) Adam Saltz, 2020
License     : GPL-3
Maintainer  : saltz.adam@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE CPP #-}
module Core.Cube(
    CubeOf(..),
    LinearCubeOf(..),
    LinearCubeOfResolutions,
    intCube,
    -- euler,
    cubePoincarePoly,
    cubePoincarePoly',
    rank,
    pruneMap,
    Grading,
    arrowRank,
    chainGroup,
    amap,
    instantiate
    )

where
import Data.Map.Lazy (Map)
import Control.Monad (replicateM)
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List
import Control.Arrow ((&&&))

import Core.Util
import Core.Resolution

-- | The ur-cube.  Returns a "cube" of dimension @n@, i.e. the @n@-th cartesian power of @[0,1]@.
intCube :: Int -> [[Int]]
intCube n = replicateM n [0,1]

-- | A cube has three parts: an @index@ which labels the vertices, @object@s which live at the vertices, and @morphism@s between vertices.  
newtype CubeOf index object morphism = CubeOf {
                                           arrows' :: Map index (Map object (Map index morphism))
                                           }
-- | A linear cube is like a 'CubeOf' but the morphisms are assumed to be linear maps over Z/2Z, see 'Algebra.V'.
newtype LinearCubeOf index object = LinearCubeOf {  
                         arrows :: Map index (Map object (Map index (Set object))) -- an arrow from a vertex is an indexed morphism
                       } deriving (Eq, Ord, Show, Read)

-- | Converts a 'CubeOf' into a 'LinearCubeOf' by changing @o -> Set o@ instead a finite @Map o (Set o)@.
instantiate :: CubeOf i o (o -> Set o) -> LinearCubeOf i o
instantiate cube = LinearCubeOf cube' where
  cube' = fmap (M.mapWithKey(\o m' -> fmap ($ o) m')) . arrows' $ cube

-- | Map over the arrows of a linear cube.
amap :: (Map i (Map o (Map i (Set o))) -> Map i (Map o (Map i (Set o)))) -> LinearCubeOf i o -> LinearCubeOf i o
amap f c = c {arrows = f . arrows $ c}

-- | @ type LinearCubeOfResolutions = LinearCubeOf Resolution@.
type LinearCubeOfResolutions = LinearCubeOf Resolution

-- | The chain group of a 'LinearCubeOfResolutions'.
chainGroup ::  LinearCubeOfResolutions o -> Map Resolution (Set o)
chainGroup =  fmap M.keysSet . arrows

-- | Computes rank.
rank ::  LinearCubeOfResolutions o -> Int
rank = sum . fmap snd . M.toList . fmap S.size . chainGroup

-- | Computes the number of "arrows" in a LinearCubeOfResolutions.  (In linear-algebraic terms: represent the morphisms in a cube by one big matrix.  @arrowRank@ is the sum of the elements of that matrix over the integers.)
arrowRank :: LinearCubeOfResolutions o -> Int
arrowRank = length . concatMap M.toList . concatMap M.elems . M.elems . pruneMap . arrows 

-- | Removes empty sets, maps, etc. from a cube.
pruneMap :: Map a (Map b (Map c (Set d))) -> Map a (Map b (Map c (Set d)))
pruneMap = M.filter (not . M.null) 
  . fmap (M.filter (not . M.null)
          . fmap (M.filter (not . S.null)))

-- | An integer grading compares two objects and returns an integer.
type Grading a b = (a,b) -> Int

-- * Euler characteristics and Poincare polynomials (not tested!)

euler2 :: Grading Resolution o -> Grading Resolution o -> LinearCubeOfResolutions o -> [(Int,Int)]
euler2 homGrading qGrading =  fmap (homGrading &&& qGrading) .  concatMap sequence .M.toList . fmap S.toList . chainGroup
  
cubePoincarePoly' :: Grading Resolution o -> Grading Resolution o -> Char -> Char -> LinearCubeOfResolutions o  -> String
cubePoincarePoly' homGrading qGrading t q =  showPoly t q . euler2 homGrading qGrading

cubePoincarePoly ::  Grading Resolution o -> Grading Resolution o -> LinearCubeOfResolutions o -> String
cubePoincarePoly hg qg = cubePoincarePoly' hg qg 't' 'q'
-- | Prints a polynomial, e.g. graded Euler characteristic.
showPoly :: Char -> Char -> [(Int,Int)] -> String
showPoly t q = intercalate " + " . fmap showTerm . coeff  where
    count :: Eq a => a -> [a] -> Int
    count x = length . filter (==x) 
    coeff :: [(Int,Int)] -> [(Int,(Int,Int))]
    coeff xs = fmap swap . graphF (`count` nub xs) $ xs
    showTerm (1,(0,0)) = "1"
    showTerm (1,(m,0)) = show t ++ "^" ++ show m 
    showTerm (1,(0,n)) = show q ++ "^" ++ show n
    showTerm (1,(m,n)) = show t ++ "^" ++ show m ++ show q ++ "^" ++ show n 
    showTerm (c,(0,0)) = show c
    showTerm (c,(m,0)) = show c ++ show t ++ "^" ++ show m 
    showTerm (c,(0,n)) = show c ++ show q ++ "^" ++ show n
    showTerm (c,(m,n)) = show c ++ show t ++ "^" ++ show m ++ show q ++ "^" ++ show n 


-- need signs, just make it its own module

