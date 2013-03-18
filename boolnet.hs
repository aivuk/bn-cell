import qualified Data.Vector.Unboxed as V
import qualified System.Random.Mersenne as R
import qualified Data.IntMap as M
import qualified Data.Array as A
import Data.Maybe (fromJust, isJust)
import Control.Concurrent 
import Control.Monad
import Graphics.UI.SDL as SDL

type Grid = A.Array Pos (Maybe BNet) 
type Pos = (Int, Int)
type BNetVertices = [BVert]
type BNetState = V.Vector Bool
data BVert = BVert { updateState :: Grid -> Pos -> BNet -> Bool
                   , updateWorld :: Grid -> Pos -> BNet -> [(Pos, Maybe BNet)]
                   , inputVerts :: V.Vector Int }
data BNet = BNet { bnState :: BNetState, bnVertices :: BNetVertices }

instance Show BNet where
    show bn = "rede"

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

zero_state = take 11 $ repeat False
one_below = (replicate 7 False) ++ [False, False, True, False]
one_right = (replicate 7 False) ++ [False, False, False, True] 
one_top = (replicate 7 False) ++ [True, False, False, False] 
one_left = (replicate 7 False) ++ [False, True, False, False]  

g |> (x,y) | x < 0 && y < 0 = g A.! (xmax, ymax)
           | x < 0 = g A.! (xmax, y `mod` (ymax + 1))
           | y < 0 = g A.! (x `mod` (xmax + 1), ymax) 
           | otherwise = g A.! (x `mod` (xmax + 1), y `mod` (ymax + 1))
    where
            (xmax,ymax) = snd $ A.bounds g

bn s0 = BNet (V.fromList s0) 
     -- Color vertices
     [bv (\g p (BNet s v) -> (not $ s V.! 3) && (s V.! 9 || s V.! 7)
      , \_ _ _ -> []
      , V.fromList [0,1]), 
      bv (\g p (BNet s v) -> (not $ s V.! 3) && (s V.! 10 || s V.! 7) 
      , \_ _ _ -> [] 
      , V.fromList [0]),
      bv (\g p (BNet s v) -> (not $ s V.! 3) && s V.! 8
      , \_ _ _ -> []       
      , V.fromList [0,1]),
     -- State vertices
     --
      bv (\g p (BNet s v) -> s V.! 3 || or (map (s V.!) [7..10]) || (and $ map ((not).(s V.!)) [7..10])
      , \g (x,y) (BNet s v) -> if (not.isJust $ g |> (x, y + 1)) -- && (and $ map ((not).(s V.!)) [7..10])
                                then 
                                    [((x, (y + 1) `mod` 21), Just $ bn one_below)] 
                                else 
                                [] 
      , V.fromList [3,7,8,9]),  

      bv (\g (x,y) (BNet s v) -> s V.! 3 || s V.! 10
      , \g (x,y) (BNet s v) -> if (not.isJust $ g |> (x - 1, y)) && s V.! 3 
                                then 
                                    [((if x > 0 then x - 1 else 20, y), Just $ bn one_right)] 
                                else 
                                    []                    
      , V.fromList [3]),   

      bv (\g (x,y) (BNet s v) -> s V.! 4 || s V.! 7
      , \g (x,y) (BNet s v) -> if (not.isJust $ g |> (x, y - 1)) && s V.! 4
                                then 
                                    [((x, if y > 0 then y - 1 else 20), Just $ bn one_top)] 
                                else
                                    []                    
      , V.fromList [4]),    

      bv (\g (x,y) (BNet s v) -> s V.! 5 
      , \g (x,y) (BNet s v) -> if (not.isJust $ g |> (x + 1, y)) && s V.! 5
                                then 
                                    [(((x + 1) `mod` 21, y), Just $ bn one_left)]
                                else 
                                    []                     
      , V.fromList [5]),     

     -- Border vertices
      bv (\g (x,y) (BNet s v) -> ((isJust $ g |> (x,y+1)) && (bnState.fromJust) (g |> (x, y+1)) V.! 9)
      , \g (x,y) (BNet s v) -> if (not.isJust $ g |> (x, y + 1)) -- && (and $ map ((not).(s V.!)) [7..10])
                                then 
                                    [((x, (y + 1) `mod` 21), Just $ bn one_below)] 
                                else 
                                []                                             
      , V.fromList [3]),      
      bv (\g (x,y) (BNet s v) -> s V.! 3 || ((isJust $ g |> (x-1,y)) && (bnState.fromJust) (g |> (x-1, y)) V.! 10)
      , \_ _ _ -> []                   
      , V.fromList [5]),       
      bv (\g (x,y) (BNet s v) -> s V.! 4 || ((isJust $ g |> (x,y-1)) && (bnState.fromJust) (g |> (x, y-1)) V.! 7)
      , \_ _ _ -> []                   
      , V.fromList [5]),        
      bv (\g (x,y) (BNet s v) -> s V.! 5 || ((isJust $ g |> (x+1,y)) && (bnState.fromJust) (g |> (x+1,y)) V.! 8)
      , \_ _ _ -> []                   
      , V.fromList [5])]


-- Draw Grid with cells

drawGrid screen grid = do    
    sequence [ drawSquare screen (i,j) (grid |> (i,j)) | i <- [0..xmax], j <- [0..ymax] ] 
    SDL.flip screen
        where drawSquare s (i,j) cell = do
                let cell_color = case cell of Nothing -> (1,1,1)
                                              Just c  -> (bi $ vertState c 2, bi $ vertState c 1, bi $ vertState c 0)
                let color (0,0,0) = 0x000000
                    color (0,0,1) = 0x0000FF
                    color (0,1,0) = 0x00FF00 
                    color (0,1,1) = 0x00FFFF 
                    color (1,0,0) = 0xFF0000  
                    color (1,0,1) = 0xFF00FF   
                    color (1,1,0) = 0xFFFF00   
                    color (1,1,1) = 0xFFFFFF
                    sSize = screen_size `div` (xmax + 1)
                    rect i j = Just $ Rect (i*sSize) ((abs $ j - 20)*sSize) sSize sSize
                SDL.fillRect screen (rect i j) (SDL.Pixel $ color cell_color)
                return ()
              (xmax, ymax) = snd $ A.bounds grid
              vertState c v = (bnState c) V.! v
              bi b = if b then 1 else 0
 

sdlUpdate screen grid step = do
    let new_grid = updateGrid grid
    threadDelay 100000
    drawGrid screen new_grid
    sdlUpdate screen new_grid (step + 1)

screen_size = 800

main = do
    SDL.init [InitEverything]
    setVideoMode screen_size screen_size 32 []
    let grid = A.array ((0,0), (20,20)) $ [((9,11), Just $ bn one_below), ((9,10), Just $ bn one_right), ((11,10), Just $ bn one_left), ((10,10), Just $ bn zero_state)] ++ [((i,j), Nothing) | i <- [0..20], j <- [0..20], (i,j) /= (10,10) && (i,j) /= (9,10) && (i,j) /= (11,10) && (i,j) /= (9,11)] :: Grid
    screen <- SDL.getVideoSurface
    forkIO . forever $ waitEvent >>= \e -> when (e == Quit) quit
    sdlUpdate screen grid 0
-- 
