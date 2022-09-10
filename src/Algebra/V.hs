{-|
Module      : Algebra.V
Description : The Khovanov chain group
Copyright   : (c) Adam Saltz, 2020
License     : GPL-3
Maintainer  : saltz.adam@gmail.com
Stability   : experimental
Portability : POSIX

Implements the Khovanov chain group (vector space) by defining a 'Labeling' of a diagram.  Also 'Ring' instances for endomorphisms of that group.
-}
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


class Ring a where
  rzero :: a -- ^ additive identity
  (<+>) :: a -> a -> a -- ^ additive operation
  (<<>>) :: a -> a -> a -- ^ multiplicative operation

-- | Sums a list of elements of a 'Ring'.  Maybe should be called 'rsum'.
rconcat :: Ring r => [r] -> r
rconcat = foldl (<+>) rzero

-- | We think of @ Map a (Set a) @ as a function between finite-dimensional
--  vector spaces with a fixed generating set and coefficients in Z/2Z.
--  The @a@'s are the generators, and @Set a@ is a linear combination.
--  Now @Map a (Set a)@ is an endomorphism (self-map) of this vector space.
--  The addition operation is the sum of linear functions, and the 
--  multiplication operator is composition of functions.
instance (Ord a) => Ring (Map a (Set a)) where
  rzero = mempty
  (<+>) = addMapMod2
  m <<>> m' = fmap (sFromListMod2 . concat . catMaybes . fmap (\k -> M.lookup k (fmap S.toList m)) . S.toList) m'

-- | Wraps @ Map Component Z2 @, labelings of the components of a diagram by 
-- 'Z2'.
newtype Labeling = Labeling {getLabeling :: Map Component Z2} deriving (Eq, Ord,Show,Generic,Read)
-- shady

-- | Again, we can think of @Labeling -> Set Labeling@ as a function
--  on a vector space with a fixed basis.  The instance is "the same" as
--  @Map a (Set a)@.  (TODO: make them explicitly the same instance.)
instance Ring (Labeling -> Set Labeling) where
    rzero = const S.empty
    (f <+> g) l = f l `addSetsMod2` g l
    f <<>> f' = unionsMod2 . fmap f . S.toList . f'
      -- temp  = fmap (catMaybesSet . S.map (f `M.lookup`)) g

-- | Delete a component from a labeling.
deleteComp :: Component -> Labeling -> Labeling
deleteComp c = Labeling . M.delete c. getLabeling

instance Semigroup Labeling where
    (<>) l l' = Labeling (M.unionWith (+) (getLabeling l) (getLabeling l'))
 
instance Monoid Labeling where
    mempty = Labeling M.empty
    mappend = (<>)

-- | Generates all possible labelings for a configuration.
generateLabelings :: Configuration -> [Labeling]
generateLabelings config = let
  labels =  replicateM (S.size .  components $ config) [ZZero, ZOne]
                           in
                             fmap (Labeling . M.fromList . zip (S.toList . components $ config)) labels   

-- | Fancy synonym for 'generateLabelings'
buildKhovanovGroup :: Configuration -> [Labeling]
buildKhovanovGroup = generateLabelings


-- | Builds a labeling from a function @ Component -> Z2 @.  For example, 
-- @
--      makeLabel (const ZZero)    
-- @
-- labels every component with 'ZZero'. 
makeLabel :: (Component -> Z2) -> [Component] -> Labeling
makeLabel f comps = Labeling $ graphMap f comps
