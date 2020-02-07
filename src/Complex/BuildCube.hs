module Complex.BuildCube
  ( subconfigurations
  , cubeOfConfigurations
  )
where
import           Core.Configuration
import           Core.PlanarDiagram
import           Core.Resolution
import           Core.Util
--import Core.Draw
import           Core.Cube

import           Data.List                      ( delete
                                                , (\\)
                                                )
import           Data.Map.Lazy                ( Map )
import qualified Data.Map.Lazy               as M

-- generates non-empty subconfigurations
subconfigurations :: Configuration -> [Configuration]
subconfigurations (Configuration pd decs c _) =
  fmap (\ds -> Configuration pd ds c (activeGraph pd ds)) (nonemptySubsets decs)

-- this is what we actually need!
-- start with a resolution and configuration at that resolution
-- relevantDecs takes two resolutions and tells you which decorations you need to get from one to the other
-- the output tells you what configurations go from res to tarRes
{-generateEdges''
  :: Resolution
  -> Configuration
  -> Map Resolution (Set Decoration)
  -> Map Resolution Configuration
generateEdges'' res conf decoMap = graphMap
  (\r -> replaceDecos conf (relevantDecs res r decoMap))
  (greaterRes res)
 where
  replaceDecos (Configuration pd _ c g) ds = Configuration pd ds c g
  relevantDecs
    :: Resolution
    -> Resolution
    -> Map Resolution (Set Decoration)
    -> Set Decoration
  relevantDecs res' tarRes decMap' =
    (decMap' M.! tarRes) `S.difference` (decMap' M.! res')
-}
greaterRes :: Resolution -> [Resolution]
greaterRes res = delete res (go res)
 where
  go :: Resolution -> [Resolution]
  go (ZeroR : res') = fmap (OneR :) (go res') ++ fmap (ZeroR :) (go res')
  go (OneR  : res') = fmap (OneR :) (go res')
  go []             = [[]]

-- note that this only considers homologically increasing morphisms!
-- could have addMorphisms which considers self-morphisms as well:
-- replace greaterRes r with r : greaterRes r
addMorphisms0
  :: Ord o => (o -> o -> m) -> Map Resolution o -> CubeOf Resolution o m
  -- -> Map Resolution (Map o (Map Resolution (Set (o, m))))
addMorphisms0 f theMap = CubeOf $ M.fromList
  [ ( r
    , M.fromList
      [ ( theMap M.! r
        , M.fromList
          [ (r', f (theMap M.! r) (theMap M.! r')) | r' <- greaterRes r ]
        )
      ]
    )
  | r <- M.keys theMap
  ]
    -- (r, M.fromList [(r', (cube1 M.! r, f (cube1 M.! r) (cube1 M.! r')))]) | r <- M.keys $ cube1, r' <- r:greaterRes r]

-- this is oriented
-- remove 0-dimensional configs!
cubeOfConfigurations
  :: CrossPD -> CubeOf Resolution DecoratedResolvedDiagram Configuration
cubeOfConfigurations pd =
  let cube1 =
        cubeOfResolutionsDecorated pd :: M.Map
            Resolution
            DecoratedResolvedDiagram
      doIt
        :: DecoratedResolvedDiagram -> DecoratedResolvedDiagram -> Configuration
      doIt sourceDD targetDD = drdToConfig $ sourceDD
        { decorationsDR = decorationsDR sourceDD \\ decorationsDR targetDD
        }
  in  addMorphisms0 doIt  cube1
