module BoolNet where

import qualified Data.Vector.Unboxed as V
import qualified System.Random.Mersenne as R
import qualified Data.IntMap as M

type BNetVertices = [((V.Vector Bool -> V.Vector Int -> Bool), V.Vector Int)]
type BNetState = V.Vector Bool
data BNet = BNet { unState :: BNetState, unVertices :: BNetVertices }

updateBNet :: BNet -> BNet
updateBNet (BNet s v) = BNet s' v
    where s' = V.fromList $ map (\(f, i) -> f s i) v

bn = BNet (V.fromList [True, False, True]) 
     [(\v li -> v V.! (li V.! 1) || v V.! (li V.! 0) && v V.! (li V.! 1), V.fromList [0,1]), 
      (\v li -> v V.! (li V.! 0), V.fromList [0]),
      (\v li -> if v V.! (li V.! 0) && v V.! (li V.! 1) then True else False, V.fromList [0,1])]
