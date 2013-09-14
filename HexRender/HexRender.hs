module HexRender.HexRender (createGrid, render, shutdown, tileFromDirection) where

import HexRender.Core.Model
import HexRender.Core.Assets
import HexRender.Core.Draw 
import qualified HexRender.Core.Grid as G

import Graphics.UI.SDL as SDL

import Math.Geometry.Grid.Hexagonal2


-- Update the Field, render changes.
render :: HexState -> IO HexState
render s@(f, _) = do
  m' <- drawField s
  return (f, m')
  
-- unload resources.
shutdown :: HexState -> IO ()
shutdown = freeField 



-- Helpful safe functions.
createGrid :: Dimensions -> Dimensions -> Dimensions -> RectHexGrid
createGrid = G.createGrid

-- Yep.
tileFromDirection :: Position -> Direction -> Field -> Maybe Tile
tileFromDirection = G.tileFromDirection