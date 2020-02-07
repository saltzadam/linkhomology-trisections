{-# LANGUAGE DeriveGeneric #-}
module Core.Grid
  ( Point(..)
  , shiftXBy
  , shiftYBy
  , findOtherVertex
  , _x
  , _y
  , negatePoint
  , Orientation(..)
  , farthest
  , checkDirection
  , opposite
  , distance
  , GridPic(..)
  , sh'
  , LineColor(..)
  , stringToGrid
  , buildGridPic
  , Edge
  )
where
import           Data.List                      ( maximumBy )
import           Data.Function                  ( on )
import           Core.Util
import           Data.Semigroup
import           GHC.Generics                   ( Generic )
--10/29: used polymorphic point
--data Point' a = Point a a deriving (Show, Eq, Ord, Functor)
--don't see a reason to, would rather unpack!
data Point = Point {-# UNPACK #-} !Int {-# UNPACK #-}  !Int deriving (Eq, Show, Ord, Generic,Read)

instance Num Point where
  (Point x y) + (Point z w) = Point (x + z) (y + w)
  (Point x y) * (Point z w) = Point (x * z) (y * w)
  abs (Point x y) = Point (abs x) (abs y)
  signum (Point x y) = Point (signum x) (signum y)
  fromInteger n = Point (fromInteger n) 0
  negate (Point x y) = Point (-x) (-y)

_x :: Point -> Int
_x (Point x _) = x
_y :: Point -> Int
_y (Point _ y) = y

shiftXBy :: Int -> Point -> Point
shiftXBy k (Point x y) = Point (x + k) y
shiftYBy :: Int -> Point -> Point
shiftYBy k (Point x y) = Point x (y + k)

negatePoint :: Point -> Point
negatePoint (Point x y) = Point (-x) (-y)
type Edge = (Point, Point)

-- data TwoByTwoMatrix a = Mat a a a a deriving Functor

-- determinant :: Num a => TwoByTwoMatrix a -> a
-- determinant (Mat a b c d) = a*d - b*c

-- divideMatBy :: Fractional a => TwoByTwoMatrix a -> a -> TwoByTwoMatrix a
-- divideMatBy (Mat a b c d) r = Mat (a/r) (b/r) (c/r) (d/r)

-- invert :: (Fractional a, Eq a) => TwoByTwoMatrix a -> Maybe (TwoByTwoMatrix a)
-- invert m@(Mat a b c d) = case determinant m of 
--     0 -> Nothing
--     det -> Just $ divideMatBy (Mat d (-b) (-c) a) det

--vecToPoint :: TwoVec -> Point
--vecToPoint (Vec x y) = Point x y

-- mulVec :: Num a => TwoByTwoMatrix a -> TwoVec a -> TwoVec a
-- mulVec (Mat a b c d) (Vec e g) = Vec (a * e + b * g) (c * e + d * g)


-- findIntersection :: Edge -> Edge -> Maybe (Point' Rational)
-- findIntersection (p,p') (q,q') = let
--                                      x0 = _x p
--                                      x1 = _x p'
--                                      y0 = _y p
--                                      y1 = _y p'
--                                      xstar0 = _x q
--                                      xstar1 = _x q'
--                                      ystar0 = _y q
--                                      ystar1 = _y q'
--                                      mat = toRational <$> 
--                                            Mat (x1 - x0) (y1 - y0) (xstar1 - xstar0) (ystar1 - ystar0) 
--                                      target = toRational <$>
--                                            Vec (x1 * y0 - x0 * y1) (xstar1 * ystar0 - xstar0 * ystar1)

--                                   in
--                                      do
--                                         inv <- invert mat
--                                         let intersectionVec = inv `mulVec` target
--                                         return $ vecToPoint intersectionVec




data SideData = SameSide | DifferentSigns | Colinear deriving Eq

onSameSide :: Edge -> Point -> Point -> SideData
onSameSide (q, r) p p' | dotQRQP * dotQRQP' == 0 = Colinear
                       | dotQRQP * dotQRQP' > 0  = SameSide
                       | otherwise               = DifferentSigns
 where
  vecQP    = differenceOfPoints p q
  vecQP'   = differenceOfPoints p' q
  vecQR    = differenceOfPoints r q
  dotQRQP  = dotProduct vecQP vecQR
  dotQRQP' = dotProduct vecQP' vecQR


-- takes an edge and two points
-- c and d lie on the same side of (a,b) return c
-- otherwise return d
findOtherVertex :: Edge -> Point -> Point -> Point
findOtherVertex (a, b) c d = if onSameSide (a, c) b d == SameSide then c else d

differenceOfPoints :: Point -> Point -> TwoVec
differenceOfPoints (Point b c) (Point d e) = Vec (b - d) (c - e)

data TwoVec = Vec {-# UNPACK #-} !Int {-# UNPACK #-} !Int

dotProduct :: TwoVec -> TwoVec -> Int
dotProduct (Vec a b) (Vec c d) = a * c + b * d

norm :: Point -> Double
norm p =
  sqrt (fromIntegral $ _x p ^ (2 :: Int) + _y p ^ (2 :: Int) :: Double)

farthest :: [Point] -> Point
farthest = maximumBy (compare `on` norm)

distance :: Point -> Point -> Double
distance (Point x y) (Point x' y') = norm (Point (x - x') (y - y'))

data ThreePointStrand = TPS Point Point Point deriving (Eq, Show)

data Orientation = CW | CCW deriving (Eq, Show, Ord)
opposite :: Orientation -> Orientation
opposite CW  = CCW
opposite CCW = CW

checkDirection :: (Point, Point, Point) -> Orientation
checkDirection = checkDirection' . parseToTPS

parseToTPS :: (Point,Point,Point) -> ThreePointStrand
parseToTPS (x, y, z) = TPS x y z


-- still feel like something is wrong here
-- just check again if the farthest point really does need to be a corner
checkDirection' :: ThreePointStrand -> Orientation
checkDirection' (TPS p q r) = --trace (show p ++ show q ++ show r) $
                              if p == r
  then CCW
  else case compare (_x q) (_x p) of
    GT -> case compare (_y r) (_y q) of
      GT -> CW
      LT -> CCW
      EQ -> error "diagonal edge somewhere in checkDirection'"
    LT -> case compare (_y r) (_y q) of
      GT -> CCW
      LT -> CW
      EQ -> error "diagonal edge somewhere in checkDirection'"
    EQ -> case compare (_y q) (_y p) of
      GT -> case compare (_x r) (_x q) of
        GT -> CCW
        LT -> CW
        EQ -> error $ "diagonal edge somewhere in checkDirection': " ++ show p ++ show q ++ show r
      LT -> case compare (_x r) (_x q) of
        GT -> CW
        LT -> CCW
        EQ -> error "diagonal edge somewhere in checkDirection'"
      EQ -> error "self-edge somewhere in checkDirection"

data GridPic = GridPic {points :: [Point],
                        edges  :: [Edge],
                        edgeToEdge :: [(Edge,Edge)]
                       } deriving Eq
instance Show GridPic where
    show  = unlines . fmap concat . sh'

instance Semigroup GridPic where
    (<>) (GridPic p e eTe) (GridPic p' e' eTe') = GridPic (p `mappend` p') (e `mappend` e') (eTe `mappend` eTe')

instance Monoid GridPic where
    mempty = GridPic [] [] []
    mappend = (<>)

stringToGrid :: [[String]] -> GridPic
stringToGrid rows = uncurry3 GridPic $ stringToGrid' 0 rows
 where
  stringToGrid' j (row' : rows') = parseRow j row' <> stringToGrid' (j + 1) rows'
  stringToGrid' _ []           = mempty
  parseRow :: Int -> [String] -> ([Point], [Edge], [(Edge, Edge)])
  parseRow y xs = go y 0 xs ([], [], [])
  go y i (x : xs) (ps, es, ds) = case even y of
    True
      | x == "." -> go y (i + 1) xs (currentPoint i y : ps, es, ds)
      | x == " " -> go y (i + 1) xs (ps, es, ds)
      | x == "-" -> go
        y
        (i + 1)
        xs
        (ps, (currentPoint i y, currentPoint (i + 2) y) : es, ds)
      | otherwise -> error
        ("not a valid grid (even): " ++ x ++ "at position " ++ show (i, y))

    False -> case even i of
      True
        | x == " " -> go y (i + 1) xs (ps, es, ds)
        | x == "|" -> go
          y
          (i + 1)
          xs
          (ps, (currentPoint i y, currentPoint i (y + 1)) : es, ds)
        | otherwise -> error
          ("not a valid grid (odd, even): " ++ x ++ "at position " ++ show
            (i, y)
          )

      False
        | x == " " -> go y (i + 1) xs (ps, es, ds)
        | x == "^" -> go y
                         (i + 1)
                         xs
                         (ps, es, (edgeBelow i y, edgeAbove i y) : ds)
        | x == "v" -> go y
                         (i + 1)
                         xs
                         (ps, es, (edgeAbove i y, edgeBelow i y) : ds)
        | x == ">" -> go y
                         (i + 1)
                         xs
                         (ps, es, (edgeLeft i y, edgeRight i y) : ds)
        | x == "<" -> go y
                         (i + 1)
                         xs
                         (ps, es, (edgeRight i y, edgeLeft i y) : ds)
        | otherwise -> error
          ("not a valid grid (odd): " ++ x ++ "at position " ++ show (i, y))
  go _ _ _ stuff = stuff
  currentPoint i y = Point (i `div` 2) (y `div` 2)
  edgeAbove i y = (currentPoint i y, currentPoint (i + 1) y)
  edgeBelow i y = (currentPoint i (y + 1), currentPoint (i + 1) (y + 1))
  edgeRight i y = (currentPoint (i + 1) y, currentPoint (i + 1) (y + 1))
  edgeLeft i y = (currentPoint i y, currentPoint i (y + 1))

buildGridPic :: [String] -> GridPic
buildGridPic = stringToGrid . fmap (fmap (: []))  -- :[] is the function \c -> [c]

data LineColor = None | Red deriving Eq
instance Show LineColor where
    show None = "None"
    show Red = "\ESC[31mRed\ESC[0m"
data LineType = Up' | Down' | Left' | Right' deriving Eq
sh' :: GridPic -> [[String]] -- NEEDS EDGES
sh' (GridPic ps es ds) =
  let allPoints = ps ++ concat [ [p, q] | (p, q) <- es ] ++ concat
        [ [r, s, t, u] | ((r, s), (t, u)) <- ds ]
      maxY  = maximum (fmap _y allPoints)
      maxX  = maximum (fmap _x allPoints)
      blank = replicate (2 * (maxY + 1)) (replicate (2 * (maxX + 1)) " ")
      lineAtPoints p q | _x p < _x q = (p, Right')
                       | _y p < _y q = (p, Up')
                       | _x p > _x q = (p, Left')
                       | _y p > _y q = (p, Down')
      drawLine :: (Point, LineType) -> ([[String]] -> [[String]])
      drawLine (p, Down' ) = alterAt (2 * _y p - 1) (replaceAt (2 * _x p) "|")
      --drawLine (p, Down') = alterAt (2* _y p + 1) (replaceAt (2* _x p ) "|")
      drawLine (p, Right') = alterAt (2 * _y p) (replaceAt (2 * _x p + 1) "-")
      drawLine (p, Up'   ) = alterAt (2 * _y p + 1) (replaceAt (2 * _x p) "|")
      drawLine (p, Left' ) = alterAt (2 * _y p) (replaceAt (2 * _x p - 1) "-")
      --drawLine (p, Left') = alterAt (2 * _y p) (replaceAt (2* _x p + 1) "-")

      drawArrow :: (Edge, Edge) -> ([[String]] -> [[String]])
      drawArrow (e, e')
        | horizontal e && horizontal e' = if e `above` e'
          then replaceOnGrid (swap $ center e e') "^"
          else replaceOnGrid (swap $ center e e') "v"
        | e `leftOf` e' = replaceOnGrid (swap $ center e e') ">"
        | otherwise = replaceOnGrid (swap $ center e e') "<"

      dotAtPoint :: Point -> ([[String]] -> [[String]])
      dotAtPoint (Point x y) = alterAt (2 * y) (replaceAt (2 * x) ".")
      horizontal :: Edge -> Bool
      horizontal (p, q) = _y p == _y q
      -- vertical :: Edge -> Bool
      -- vertical (p, q) = _x p == _x q
      above :: Edge -> Edge -> Bool
      h `above` h' = (_y . fst $ h) > (_y . fst $ h') -- only for horizontal
      leftOf :: Edge -> Edge -> Bool
      h `leftOf` h' = (_x . fst $ h) < (_x . fst $ h')
      replaceOnGrid (x, y) f = alterAt x (replaceAt y f)
  in  compose (fmap drawArrow ds)
      . compose (fmap (drawLine . uncurry lineAtPoints) es)
      . compose (fmap dotAtPoint ps)
      $ blank

center :: Edge -> Edge -> (Int, Int)
center (Point a b, Point c d) (Point e f, Point g h) =
  ((a + c + e + g) `div` 2, (b + d + f + h) `div` 2)
