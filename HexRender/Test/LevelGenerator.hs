module HexRender.Test.LevelGenerator(randomLevel) where

import System.Random

import Data.Map as M
import Data.List as L
import Data.Maybe
import Math.Geometry.Grid as G
import Math.Geometry.Grid.Hexagonal2

import HexRender.Core.Model as HexModel
import HexRender.Test.GameModel

import HexRender.Test.Data

data CAState = Dead | Alive
             deriving (Eq)

type CellularAutomata = M.Map Position CAState

randomLevel :: HexGrid -> IO (M.Map Position GameTile)
randomLevel grid = do
  g <- getStdGen
  -- randomize everything.
  let (ca, g') = L.foldl' randomCAState (M.empty, g) innerPoses
  -- force diagonals to be Dead.
  let dCA = crossGrid grid edgePoses ca
  -- force borders to be Alive
  let bCA = L.foldl' (\ca' k -> M.insert k Alive ca') dCA edgePoses
      
  let finalCA = runCA bCA $ b56s23456 grid
  
  return $ snd $ M.mapAccumWithKey populateLevel g' finalCA
  where
    edgePoses = G.boundary grid

    innerPoses = G.indices grid L.\\ edgePoses
    
------------------------------------------        
randomCAState :: (CellularAutomata,StdGen) -> Position -> (CellularAutomata, StdGen)
randomCAState (ca, g) p = (M.insert p s ca, g')
  where
    s
      | r < 45 =    Alive
      | otherwise = Dead
                    
    (r, g') = randomR (0,100) g :: (Int, StdGen)
    
----------------------------------------
runCA :: CellularAutomata -> (CellularAutomata -> Position -> CAState -> CAState) -> CellularAutomata
runCA ca f =
  if ca' /= ca
  then runCA ca' f
  else ca
  where
    ca' = M.mapWithKey (f ca) ca

-----
b56s23456 :: HexGrid -> CellularAutomata -> Position -> CAState -> CAState
b56s23456 g m p state =
  case state of
    Alive -> if adjCellsLiving >= 1
             then Alive
             else Dead
    Dead -> if adjCellsLiving >= 5
            then Alive
            else Dead
  where
    adjCellsLiving = length $ L.filter (==Alive) adjCells
    adjCells = L.map (\k -> fromJust $ M.lookup k m) $ G.neighbours g p
--------------------------------------

-- inserts a diagonal dead cross into the CA map.
crossGrid :: HexGrid -> [Position] -> CellularAutomata -> CellularAutomata
crossGrid grid edgePoses ca =
  L.foldl' (\ca' k -> M.insert k Dead ca') ca $ L.union botLeftToTopRight topLeftToBotRight
  where
    (rows,cols) = G.size grid
    botLeft = (0,0)
    topRight = L.foldl' max (0,0) edgePoses
    botLeftToTopRight =
      head $ G.minimalPaths grid botLeft topRight
    
    topLeft = (0, rows-1)
    
    -- lol......
    botRight = L.foldl' (\p@(x,y) p'@(x',y') -> 
                         if x' > x || (x' == x && y' < y)
                         then p'
                         else p) (0,0) edgePoses
    topLeftToBotRight =
      head $ G.minimalPaths grid topLeft botRight


    







----
finalizeTile :: (Position, GameTile) -> GameTile
finalizeTile (p, gt) = gt { gtTile = (gtTile gt) { tPosition = p} }
---




populateLevel :: StdGen -> Position -> CAState -> (StdGen, GameTile)
populateLevel g p s = (g', gt')
  where
    gt' = finalizeTile (p, gt)
    (g', gt)
      | s == Alive = (g, rockTile)
      | s == Dead =  randomTile g
                     
randomTile :: StdGen -> (StdGen, GameTile)
randomTile g = (g', gt)  
  where
    (r, g') = randomR (0, 100) g :: (Int, StdGen)
    gt
      | r < 15 = lavaTile
      | r < 40 = waterTile
      | otherwise = grassTile
                    
                    
                    


-- old shit, cool syntax.
{-                      
randomTiles :: [Position] -> IO [(Position, GameTile)]
randomTiles ps  = do
  g <- getStdGen
  let psts = zip ps $ fst $ rResources (length ps) g
  mapM createTile psts
  where
    createTile (pos, gt) = 
      return (pos, gt { gtTile = (gtTile gt) { tPosition = pos }})

rResources :: Int -> StdGen -> ([GameTile], StdGen)
rResources n = runState (replicateM n (state rResource))

rResource :: StdGen -> (GameTile, StdGen)
rResource g = (gt, g')
  where
    (r, g') = randomR (0, 100) g :: (Int, StdGen)
    gt
      | r < 5 = lavaTile
      | r < 6 = rockTile
      | r < 20 = waterTile
      | otherwise = grassTile
    
-}