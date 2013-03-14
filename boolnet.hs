module BoolNet where

import qualified Data.Vector.Unboxed as V
import qualified System.Random.Mersenne as R
import qualified Data.IntMap as M
import qualified Data.Array as A
import Data.Maybe (fromJust, isJust)
import Control.Concurrent 
import Graphics.UI.SDL as SDL

type Grid = A.Array Pos (Maybe BNet) 
type Pos = (Int, Int)
type BNetVertices = [BVert]
type BNetState = V.Vector Bool
data BVert = BVert { updateState :: Grid -> Pos -> BNet -> Bool
                   , updateWorld :: Grid -> Pos -> BNet -> [(Pos, Maybe BNet)]
                   , inputVerts :: V.Vector Int }
data BNet = BNet { bnState :: BNetState, bnVertices :: BNetVertices }

updateBNet :: Grid -> Pos -> BNet -> BNet
updateBNet g p bn@(BNet s v) = bn { bnState = s' }
    where s' = V.fromList $ map (\(BVert f _ i) -> f g p bn) v

changeGridBNet :: Grid -> Pos -> BNet -> [(Pos, Maybe BNet)]
changeGridBNet g p bn@(BNet s v) = concatMap (\(BVert _ cw i) -> cw g p bn) v

updateGrid :: Grid -> Grid
updateGrid grid = grid A.// (concatMap (\(p, c) -> (changeGridBNet grid p c) ++ [(p, Just $ updateBNet grid p c)]) cells)
    where 
        cells = map (\(p,c) -> (p, fromJust c)) $ filter (\(p, c) -> isJust c) $ A.assocs grid 

bv (us, uw, iv) = BVert us uw iv

bn = BNet (V.fromList $ take 10 $ repeat False) 
     -- Color vertices
     [bv (\g p v -> v V.! 1 || v V.! 0 && v V.! 1
      , \_ _ _ -> []
      , V.fromList [0,1]), 
      bv (\g p v -> v V.! 0
      , \_ _ _ -> [] 
      , V.fromList [0]),
      bv (\g p v -> if v V.! 0 && v V.! 1 then True else False
      , \_ _ _ -> []       
      , V.fromList [0,1]),
     -- State vertices
      bv (\g p v -> v V.! 3 || (not.and) $ map (v V.!) [6..9]
      , \_ _ _ -> []             
      , V.fromList [3,6,7,8,9]),  
      bv (\g p v -> v V.! 3
      , \_ _ _ -> []                   
      , V.fromList [3]),   
      bv (\g p v -> v V.! 4
      , \_ _ _ -> []                   
      , V.fromList [4]),    
      bv (\g p v -> v V.! 5
      , \_ _ _ -> []                   
      , V.fromList [5]),     
     -- Border vertices
      bv (\g p v -> v V.! 3
      , \_ _ _ -> []                   
      , V.fromList [3]),      
      bv (\g p v -> v V.! 5
      , \_ _ _ -> []                   
      , V.fromList [5]),       
      bv (\g p v -> v V.! 5
      , \_ _ _ -> []                   
      , V.fromList [5]),        
      bv (\g p v -> v V.! 5
      , \_ _ _ -> []                   
      , V.fromList [5])]
   

-- Draw Grid with cells

--drawGrid screen grid = do    
--    sequence [ drawSquare screen i j | i <- [0..n - 1], j <- [0..n - 1] ] 
--    SDL.flip screen
--        where drawSquare s i j = do
--                s <- grid |> (i,j)
--                let color 1 = 0xFF0000
--                    color _ = 0x00FF00
--                    sSize = screen_size `div` n
--                    rect i j = Just $ Rect (i*sSize) (j*sSize) sSize sSize
--                SDL.fillRect screen (rect i j) (SDL.Pixel $ color s)
--                return ()
-- 
--
--sdlUpdate screen grid step
--   | t < 0     = return ()
--   | otherwise = do
--    let new_grid = undefined
--    drawGrid screen new_grid
--    threadDelay 1
--    sdlUpdate screen grid (c + 1)

--screen_size = 400
--
--main = do
--    SDL.init [InitEverything]
--    setVideoMode screen_size screen_size 32 []
--    let grid = undefined
--    screen <- SDL.getVideoSurface
--    forkIO . forever $ waitEvent >>= \e -> when (e == Quit) quit
--    sdlUpdate screen grid 0
-- 
