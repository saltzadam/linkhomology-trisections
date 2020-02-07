module Algebra.V
(Labeling(..),
generateLabelings,
  buildKhovanovGroup,
  makeLabel,
  Ring(..),
  rconcat,
  deleteComp)
where
import Algebra.Z2
import Core.Configuration
import Core.PlanarDiagram
import Core.Util
import Core.CubeUtil

import Control.Monad (replicateM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Semigroup
import GHC.Generics (Generic)
import Data.Maybe (catMaybes)

-- type DumbComponent = [Point]

-- dumb :: Component -> DumbComponent
-- dumb = S.toList . S.fromList . getPoints
class Ring a where
  rzero :: a -- additive identity
  (<+>) :: a -> a -> a -- additive
  (<<>>) :: a -> a -> a -- multiplicative

rconcat :: Ring r => [r] -> r
rconcat = foldl (<+>) rzero

instance (Ord a) => Ring (Map a (Set a)) where
  rzero = mempty
  (<+>) = addMapMod2
  m <<>> m' = fmap (sFromListMod2 . concat . catMaybes . fmap (\k -> M.lookup k (fmap S.toList m)) . S.toList) m'



newtype Labeling = Labeling {getLabeling :: Map Component Z2} deriving (Eq, Ord,Show,Generic,Read)
-- shady
-- instance Eq Labeling where
--   (==) l l' = M.mapKeys unoriented (getLabeling l) ==  M.mapKeys unoriented (getLabeling l') where
--     unoriented = S.fromList . getPoints 

-- instance Ord Labeling where
--   compare l l' = let label =  getLabeling l
--                      label' = getLabeling l'
--                  in
--                      if l == l' then EQ else compare label label'

-- instance Ord Labeling where
--   compare l l' = compare (M.mapKeys unoriented $ getLabeling l) (M.mapKeys unoriented $ getLabeling l')
--     where
--       unoriented c = S.fromList . getPoints $ c

instance Ring (Labeling -> Set Labeling) where
    rzero = const S.empty
    (f <+> g) l = f l `addSetsMod2` g l
    f <<>> f' = unionsMod2 . fmap f . S.toList . f'
      -- temp  = fmap (catMaybesSet . S.map (f `M.lookup`)) g

deleteComp :: Component -> Labeling -> Labeling
deleteComp c = Labeling . M.delete c. getLabeling

-- newtype DumbLabeling = DumbLabeling {getDumbLabeling :: Map (Set Point) Z2} deriving (Eq, Show, Generic, Read, Ord)

-- instance Ring (DumbLabeling -> Set DumbLabeling)
--  where
--   rzero = const S.empty
--   (f <+> g) l = f l `addSetsMod2` g l
--   f <<>> f' = unionsMod2 . fmap f . S.toList . f'

instance Semigroup Labeling where
    (<>) l l' = Labeling (M.unionWith (+) (getLabeling l) (getLabeling l'))
 
instance Monoid Labeling where
    mempty = Labeling M.empty
    mappend = (<>)

generateLabelings :: Configuration -> [Labeling]
generateLabelings config = let
  labels =  replicateM (S.size .  components $ config) [ZZero, ZOne]
                           in
                             fmap (Labeling . M.fromList . zip (S.toList . components $ config)) labels   

buildKhovanovGroup :: Configuration -> [Labeling]
buildKhovanovGroup = generateLabelings

makeLabel :: (Component -> Z2) -> [Component] -> Labeling
makeLabel f comps = Labeling $ graphMap f comps
