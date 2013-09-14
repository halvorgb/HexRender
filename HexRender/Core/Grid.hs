module HexRender.Core.Grid (createGrid, tileToScreenCoordinate, filterIllumination, tileFromDirection) where

import Math.Geometry.Grid.Hexagonal2
import Math.Geometry.Grid as G

import HexRender.Core.Model as HexModel

import Data.List as L
import Data.Map as M

-- "safely" creates a grid, not used by anything yet.
createGrid :: Dimensions -> Dimensions -> Dimensions -> RectHexGrid
createGrid gridDimensions@(rows, cols) fieldDimensions@(fdx, fdy) tileDimensions@(tdx, tdy)
  | gridFits && square  = rectHexGrid rows cols
  | otherwise = error "Error: Couldn't create grid, did not pass the dimension check. (Probably not enough room given to render the grid)"
  where
    xGrowth = fromIntegral tdx - (fromIntegral tdx/ (2.0 * sqrt 3))
    minWidth = round ((fromIntegral cols) * xGrowth)
    minHeight = rows * tdy + div tdy 2
    
    gridFits = fdx > minWidth && fdy > minHeight
    square = tdx == tdy
    
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
    x = round (fromIntegral (tx) * xGrowth) + fx
    y = (rows - 1 - ty)*tdy + (1-tx)*halftdy + fy
    
    outOfBounds = 
      x + tdx > (fx + fdx) || 
      y + tdy > (fy + fdy) ||
      x < fx ||
      y < fy
      
      
    
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
                          Object {oLightMask = LightSource r} ->  L.union ps $ lightRadius p g r tm
                          _ -> ps
                      ) [] $ M.toList om
             
    otPoses = L.foldl (\ps (p, t) -> case t of
                          Tile {tLightMask = LightSource r} -> L.union ps $ lightRadius p g r tm
                          _ -> ps
                      ) oPoses $ M.toList tm
                       

-- returs a list of tiles illuminated by the first tile
-- TODO: Logic for opaque tiles etc.
lightRadius :: Position -> RectHexGrid -> Int -> M.Map Position Tile ->  [Position]
lightRadius origin grid strength  tileMap =
  lightRadii [] [origin] grid strength gridSize tileMap
  where
    gridSize = G.size grid
    
-- Expands to neighbours for each strength of original lightSource.
-- Future: Handle lightsources encountered here? cheaper perhaps
lightRadii :: [Position] -> [Position] -> RectHexGrid -> Int -> Dimensions -> M.Map Position Tile -> [Position]
lightRadii pP                nP            _     1        _       _        = L.union pP nP
lightRadii previousPositions nextPositions grid strength gridSize tileMap
  | L.null nonOpaque = L.union previousPositions nextPositions
  | otherwise = 
    lightRadii (nextPositions ++ previousPositions) unvisited grid (strength-1) gridSize tileMap
  where
    nonOpaque = L.filter (\p -> case M.lookup p tileMap of
                             Just (Tile {tLightMask = Opaque }) -> False
                             _ -> True
                         ) nextPositions
                
                
    neighbourTiles = L.foldl' (\ps p -> L.union ps $ neighbours grid p) [] nonOpaque
    unvisited = neighbourTiles L.\\ previousPositions
