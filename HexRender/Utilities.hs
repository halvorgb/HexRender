module HexRender.Utilities where

import qualified Data.Map as M
import Math.Geometry.Grid.Hexagonal2

import HexRender.Core.Model as HexModel


-- returns the tile that can be reached from the input tile given the direction, if it is in the grid
tileFromDirection :: Position -> HexModel.Direction -> Field -> Maybe Tile
tileFromDirection origin@(x,y) direction f =
  M.lookup target tiles
  where
    tiles = fTiles f
    target = case direction of
      Up ->   (x,y+1)
      Down -> (x, y-1)
      
      UpRight ->  (x+1,y)
      DownLeft -> (x-1, y)
      
      DownRight -> (x+1, y-1)
      UpLeft ->    (x-1, y+1)
      
-- "safely" creates a grid, not used by anything yet.
createGrid :: Dimensions -> Dimensions -> Dimensions -> HexGrid
createGrid gridDimensions@(rows, cols) fieldDimensions@(fdx, fdy) tileDimensions@(tdx, tdy)
  | gridFits && square  = rectHexGrid rows cols
  | otherwise = error "Error: Couldn't create grid, did not pass the dimension check. (Probably not enough room given to render the grid)"
  where
    xGrowth = fromIntegral tdx - (fromIntegral tdx/ (2.0 * sqrt 3))
    minWidth = round (fromIntegral cols * xGrowth)
    minHeight = rows * tdy + div tdy 2
    
    gridFits = fdx > minWidth && fdy > minHeight
    square = tdx == tdy