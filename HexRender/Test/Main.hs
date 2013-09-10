module HexRender.Test.Main(main) where

import Data.Map as M
import Graphics.UI.SDL as SDL
import Math.Geometry.Grid.Hexagonal
import Math.Geometry.Grid
import System.Random
import Control.Monad.Trans.State
import Control.Monad
import Data.List as L

import HexRender.HexRender
import HexRender.Core.Model
import HexRender.Test.Tiles



main :: IO ()
main = do
  SDL.init [ SDL.InitEverything ]
  SDL.enableUnicode True
  
  setVideoMode 1600 800 32 []
  
  setCaption "HexRender!" "hexRender."
  
  mainSurf <- SDL.getVideoSurface
  
  SDL.flip mainSurf
  
  tiles <- randomTiles $ (indices grid)
  
  s' <- render (setupTestField mainSurf (M.fromList tiles) grid, M.empty)
  
  --
  waitEventBlocking >>= handleInput
  --
  shutdown s'
  SDL.quit
  where
    grid = hexHexGrid 8
    
    handleInput e = case e of
      Quit -> return ()
      _ -> waitEventBlocking >>= handleInput
      
      
setupTestField :: SDL.Surface -> M.Map Position Tile -> HexHexGrid ->  Field
setupTestField surf tiles grid  = Field { fFieldDimensions = (992, 992),
                                    fFieldSurface = surf,
                                    fFieldPosition = (32, 32),
                                    fTileDimensions = (64, 64),
                                    fTiles = tiles,
                                    fObjects = M.empty,
                                    fGrid = grid,
                                    fBackground = Primitive (0, 0, 0, 0),
                                    fTileBorder = NoBorder
                            }
                      
                      
randomTiles :: [Position] -> IO [(Position, Tile)]
randomTiles ps  = do
  g <- getStdGen
  let psts = zip ps $ fst $ rResources (length ps) g
  mapM createTile psts
  where
    createTile (pos,tile) = return (pos, tile { tPosition = pos })

rResources :: Int -> StdGen -> ([Tile], StdGen)
rResources n = runState (replicateM n (state rResource))

rResource :: StdGen -> (Tile, StdGen)
rResource g
  | r < 5 = (lavaTile, g')
  | r < 40 = (rockTile, g')
  | r < 65 = (waterTile, g')
  | otherwise = (grassTile, g')
  where
    (r, g') = randomR (0, 100) g :: (Int, StdGen)
