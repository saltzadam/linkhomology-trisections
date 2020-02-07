module LinkHomology.ReidemeisterTwo where

import           LinkHomology.LinkHomology
import           LinkHomology.Cobordism
import           Core.Configuration      hiding ( Vertical
                                                , Horizontal
                                                )
import           Core.PlanarDiagram
import           Core.Grid
import           Core.Util
import           Core.Resolution
import           Algebra.V

import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import Control.Arrow (first)
import Data.List (elemIndex)
import Data.Maybe (fromJust)



data CrossDeco' a  = CrossDeco' (CrossNode a) (CrossNode a)  deriving (Eq, Ord, Show, Functor)
type CrossDeco = CrossDeco' Point

-- The assumption here is that CrossDeco extends downwards between crossings with no crossings between them
-- We need to
-- -- cancel the crossing diagrammatic ✓ 
-- -- define a map which
-- -- -- checks if the CrossDeco intersects a single component
-- -- --        OR if it intersects no components
-- -- -- applies the appropriate map (to the appropriate complex!)

-- NOTE: does not check that the crossings have opposite signs!

-- remember that Cross _ a b c d is
-- a b
-- c d
-- so you just resolve to CrossJoin a c, CrossJoin b decoToCobordism

r2 :: LinkHomologyTheory -> CrossDeco -> CrossPD -> MapOfLinearCubes
r2 lht cdec@(CrossDeco' n n') cpd = let
  c1 = chainComplex lht cpd
  c2 = chainComplex lht (r2CPD cpd cdec)
  
  (i,j) = case elemIndex n cpd of
    Nothing -> error $ "tried to R2 between " ++ show n ++ " and " ++ show n' ++ " but " ++ show n ++ " isn't in the diagram!"
    Just _ -> case elemIndex n' cpd of
      Nothing -> error $ "tried to R2 between " ++ show n ++ " and " ++ show n' ++ " but " ++ show n' ++ " isn't in the diagram!"
      Just _ -> (fromJust . elemIndex n . filter isCross $ cpd, fromJust . elemIndex n' . filter isCross $ cpd)
  in
    makeMap c1 c2 (r2' lht cdec cpd (i,j))

-- again, no real safety here
r2CPD :: CrossPD -> CrossDeco -> CrossPD
r2CPD cpd (CrossDeco' (CrossJoin a b) (CrossJoin c d)) =
  decorationActionCrossPD (Decoration (Join a b) (Join c d)) cpd
r2CPD cpd d@(CrossDeco' cr cr') = cpd'
 where
  cpd'' = fmap (: []) cpd
  o     = determineOrientation d
  cpd' =
    concat
      . replaceBy [cr]  (braidlikeResolve o cr)
      . replaceBy [cr'] (braidlikeResolve o cr')
      $ cpd''

r2' lht cdec cpd (i,j) (r,l) = r2Map lht cdec cpd (determineR2Type (resolvePD cpd r) cdec) (i,j) (r,l)

r2Map
  :: LinkHomologyTheory
  -> CrossDeco
  -> CrossPD
  -> R2Type
  -> (Int, Int)
  -> (Resolution, Labeling)
  -> Set (Resolution, Labeling)
r2Map lht dec conf BraidLike location (r, l) =
  S.singleton (deleteResAt r location, l)-- just delete the resolution parts
    -- use twoHandleAttachment, oneHandleAttachment, then deleteResAt
    -- should check that the dec only hits one component
    -- throw an error otherwise!
r2Map lht (CrossDeco' (Cross _ a b _ _) (Cross _ a' b' _ _)) cpd AntiBraidLike location (r, l)
  = let pd         = resolvePD cpd r
        candidates = decoIntersectsComps dec' (componentsAsNodes pd)
        dec'       = Decoration (Join a b) (Join a' b')
        c          = if length candidates == 1
          then head candidates
          else
            error
            $  "R2 decoration intersects "
            ++ show (length candidates)
            ++ " components instead of one."
    in  S.map (first (`deleteResAt` location))
        . S.unions
        . S.toList . S.map (oneHandleAttachment' lht dec' cpd)
        . twoHandleAttachment' c
        $ (r, l)


deleteResAt :: Resolution -> (Int, Int) -> Resolution
deleteResAt = flip deleteAtPair

data R2Type = BraidLike | AntiBraidLike deriving (Eq, Ord, Show)
-- TODO: use getPoints more
determineR2TypeConfig :: Configuration -> CrossDeco -> R2Type
determineR2TypeConfig conf = determineR2Type (getPD conf)

determineR2Type :: PD -> CrossDeco -> R2Type
determineR2Type pd cd@(CrossDeco' c1 c2) =
  let o    = determineOrientation cd
      blr1 = unsafeCrossToFlat . braidlikeResolve o $ c1 :: PD
      blr2 = unsafeCrossToFlat . braidlikeResolve o $ c2 :: PD
  in  if all (`elem` pd) blr1 && all (`elem` pd) blr2
        then BraidLike
        else AntiBraidLike


-- one issue is what to do with the Resolutions.
-- should move this to a module entirely about pairing diagrams...

braidlikeResolve :: CrossDecoOrientation -> CrossNode Point -> [CrossNode Point]
braidlikeResolve _          (CrossJoin a b  ) = [CrossJoin a b]
braidlikeResolve Vertical   (Cross _ a b c d) = [CrossJoin a c, CrossJoin b d]
braidlikeResolve Horizontal (Cross _ a b c d) = [CrossJoin a b, CrossJoin c d]

data CrossDecoOrientation = Horizontal | Vertical deriving (Eq, Ord, Show)

-- TODO: this is a good trick, might be useful elsewhere.  "center of mass"
determineOrientation :: CrossDeco -> CrossDecoOrientation
determineOrientation (CrossDeco' (Cross _ a b c d) (Cross _ e f g h)) =
  let averageX1 = sum . fmap (\p -> fromIntegral (_x p) / 4) $ [a, b, c, d]
      averageX2 = sum . fmap (\p -> fromIntegral (_x p) / 4) $ [e, f, g, h]
  in  if averageX1 == averageX2 then Vertical else Horizontal

