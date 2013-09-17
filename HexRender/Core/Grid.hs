module HexRender.Core.Grid (tileToScreenCoordinate, filterIllumination) where

import Math.Geometry.Grid.Hexagonal2
import Math.Geometry.Grid as G

import HexRender.Core.Model as HexModel
import HexRender.Utilities

import Data.List as L
import Data.Map as M
import Data.Maybe    
-- når man blitter til en surface som ikke er like stor som "videosurface", bruker man absolutte posisjoner eller relative posisjoner?
-- TODO: Ta høyde for at ikke hele fieldet ikke vises samtidig? (Wrapping)
tileToScreenCoordinate :: Field -> Position -> Position
tileToScreenCoordinate field tilePos@(tx, ty)
  | not $ contains grid tilePos = error $ "Error: Tile position not in the field GRID, tx: " ++ show tx ++ ", ty: " ++ show ty ++ "gridSize: " ++ show gridSize ++ "."
  | outOfBounds = error $ "Error: Tile coordinates out of bounds, x: " ++ show x ++ ", y: " ++ show y ++ ", fdx: " ++ show fdx ++ ", fdy: " ++ show fdy ++ ", fx: " ++ show fx ++ ", fy: " ++ show fy ++ "."
  | tdx /= tdy = error "Error: X and Y tile dimensions differ, not a 'perfect' hexagon." -- redundant check if createGrid was used. (How to force?)
  | otherwise = (x,y)
  where
    fieldPos@(fx, fy) = fFieldPosition field
    fieldDim@(fdx, fdy) = fFieldDimensions field
    tileDim@(tdx, tdy) = fTileDimensions field
    
    grid = fGrid field
    gridSize@(rows, cols) = G.size grid
    
    -- absolute positions, using integer division - assumes even tile dimensions.
    halftdy = div tdy 2
    
    -- https://github.com/mhwombat/grid/wiki/Hexagonal-tiles-%28alternative-orientation%29 rectangular.
    -- assuming tdx == tdy
    xGrowth = fromIntegral tdx - (fromIntegral tdx / (2.0 * sqrt 3))
    x = round (fromIntegral tx * xGrowth) + fx
    y = (rows - 1 - ty)*tdy + (1-tx)*halftdy + fy
    
    outOfBounds = 
      x + tdx > (fx + fdx) || 
      y + tdy > (fy + fdy) ||
      x < fx ||
      y < fy
      
      
      
-- returns a field with non-illuminated tiles filtered out.
filterIllumination :: Field -> Field
filterIllumination f =
  f {
    fObjects = M.filterWithKey (\p _ -> elem p otPoses) om,
    fTiles = M.filterWithKey (\p _ -> elem p otPoses) tm
    }
  where
    om = fObjects f
    tm = fTiles f
    g =  fGrid f
    
    oPoses = L.foldl' (\ps (p,o) -> case o of
                          Object {oLightMask = LightSource r} ->  L.union ps $ visionRadius p g r f
                          _ -> ps
                      ) [] $ concatMap (\(p, os) -> zip (repeat p) os) $ M.toList om
             
    otPoses = L.foldl' (\ps (p, t) -> case t of
                          Tile {tLightMask = LightSource r} -> L.union ps $ visionRadius p g r f
                          _ -> ps
                      ) oPoses $ M.toList tm
                       




visionRadius :: Position -> HexGrid -> Int -> Field -> [Position]
visionRadius origin@(x,y) grid strength f =
  L.foldl' (\ts t -> if visionBlocked (minimalPaths grid origin t) f
                    then ts
                    else t:ts
          ) [origin] allVisible
  where
    tempGrid = hexHexGrid strength
    allVisible = L.filter (contains grid) $ L.map (\(x',y') -> (x+x', y+y')) $ indices tempGrid
    
visionBlocked :: [[Position]] -> Field -> Bool
visionBlocked [] _ = True
visionBlocked (path:paths) f =
  if blocked 
  then visionBlocked paths f
  else False
  where
    blocked = any (\p -> 
                    case M.lookup p $ fTiles f of
                      Just t -> tLightMask t == Opaque
                      _ -> False
                  )  $ init path