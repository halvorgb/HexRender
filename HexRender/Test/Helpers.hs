module HexRender.Test.Helpers where

import Data.Map as M

import HexRender.Core.Model as HexModel
import HexRender.Test.GameModel

-- returns the tile that can be reached from the input tile given the direction, if it is in the grid
gameTileFromDirection :: Position -> HexModel.Direction -> Game -> Maybe GameTile
gameTileFromDirection origin@(x,y) direction g =
  M.lookup target tiles
  where
    tiles = gGameTiles g
    target = case direction of
      Up ->   (x,y+1)
      Down -> (x, y-1)
      
      UpRight ->  (x+1,y)
      DownLeft -> (x-1, y)
      
      DownRight -> (x+1, y-1)
      UpLeft ->    (x-1, y+1)
