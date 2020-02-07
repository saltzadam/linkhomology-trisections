module Main
where
import Eg.Eg
import LinkHomology.LinkHomology
import Core.Braids
import Core.Cube
import Complex.Homology
import LinkHomology.Szabo
import Eg.Eg
import LinkHomology.Closure3
import LinkHomology.Closure2
import LinkHomology.Closure1
import Complex.Hyperbox
import LinkHomology.SSS
import LinkHomology.BarNatan
import Core.Draw
import Algebra.Z2
import qualified Data.Set as S
import qualified Data.Map as M

-- TODO: once it works:
-- TODO: do not use fmap,traverse,etc. with Data.Map.Strict
-- TODO: export lists
-- TODO: document


-- niceMap h x = hPutStrLn h (show $ classifyConfiguration x) >> hPutStrLn h (drawConfig x)

                 
main :: IO ()
main = do
--  mapM_ drawIO . S.toList $ closure3' sss om1 om2 om3
  -- print $ closure3 bn om1 om2 om3
  -- print $ configAndComps3 (0,0,0) (ZZero,ZZero,ZZero) om1 om2 om3

{-
  print $ closure3 sss om1 om2 om3
  print $ doubles sss om1 om2 om3
  print $ tripleProducts sss om1 om2 om3

  For the interesting diagram (spun (?) tref): 0 + (3 + 0 + 4) + 0 = 1 -}
{- Reverse of above -}
--  print $ closure3 sss om3 om2 om1
--  print $ doubles sss om3 om2 om1
--  print $ tripleProducts sss om3 om2 om1



{-  print $ closure3 sss omt omt omt
  print $ doubles sss omt omt omt
  print $ tripleProducts sss omt omt omt
  
  As expected, also gives 1
  -}

  -- print ((rank . homology . chainComplex sz . markovClosure $ Braid 3 [1,1,1,2]))
  print (( rank . homology . chainComplex sz . markovClosure $ Braid 3 [1,1,1,2,1,1,1,2]))
  -- print ((rank . homology . chainComplex sz . markovClosure $ Braid 4 [-1,-1,-1,2,1,1,-3,2,-3]) == 14)
  -- print ((szaboRank . markovClosure $ Braid 3 [1,1,1,1,1,2,1,1,1,2]) == 2)
  -- print ((szaboRank . markovClosure $ Braid 4 [1,1,1,2,1,1,2,2,3,-2,3]) == 22)
  -- print ((szaboRank . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 10) -- 10_132
  -- print ((szaboRank . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 10) -- 10_136
  -- print ((szaboRank . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 10) -- 10_139
  -- print ((szaboRank . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 10) -- 10_145
  -- print ((szaboRank . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 10) -- 10_152
  -- print ((szaboRank . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 10) -- 10_153
  -- print ((szaboRank . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 10) -- 10_154
  -- print ((szaboRank . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 10) -- 10_161

  -- print ((rank . homology . sssCube . markovClosure $ Braid 3 [1,1,1,2]) == 2)
  -- print ((rank . homology . sssCube . markovClosure $ Braid 3 [1,1,1,2,1,1,1,2]) == 2)
  -- print ((rank . homology . sssCube . markovClosure $ Braid 4 [-1,-1,-1,2,1,1,-3,2,-3]) == 2)
  -- print ((rank . homology . sssCube . markovClosure $ Braid 3 [1,1,1,1,1,2,1,1,1,2]) == 2)
  -- print ((rank . homology . sssCube . markovClosure $ Braid 4 [1,1,1,2,1,1,2,2,3,-2,3]) == 2)
  -- print ((rank . homology . sssCube . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 2) -- 10_132
  -- print ((rank . homology . sssCube . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 2) -- 10_136
  -- print ((rank . homology . sssCube . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 2) -- 10_139
  -- print ((rank . homology . sssCube . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 2) -- 10_145
  -- print ((rank . homology . sssCube . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 2) -- 10_152
  -- print ((rank . homology . sssCube . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 2) -- 10_153
  -- print ((rank . homology . sssCube . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 2) -- 10_154
  -- print ((rank . homology . sssCube . markovClosure $ Braid 4 [1,1,1,-2,-1,-1,-2,-3,2,-3,-3]) == 2) -- 10_161

            
