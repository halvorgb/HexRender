module HexRender.Core.Grid (createGrid, tileToScreenCoordinate, filterIllumination) where

import Math.Geometry.Grid.Hexagonal
import Math.Geometry.Grid as G

import HexRender.Core.Model

import Data.List as L
import Data.Map as M

-- "safely" creates a grid, not used by anything yet.
createGrid :: Int -> Dimensions -> Dimensions -> HexHexGrid
createGrid s fieldDimensions@(fx, fy) tileDimensions@(tx, ty)
  | gridFits && square  = hexHexGrid s
  | otherwise = error "Error: Couldn't create grid, did not pass the dimension check. (Probably not enough room given to render the grid)"
  where
    minWidth = ((s-1)*2 + 1) * fx
    minHeight = ((s-1)*2 + 1) * fy
    
    gridFits = fx > minWidth && fy > minHeight
    square = fx == fy && tx == ty
    
-- når man blitter til en surface som ikke er like stor som "videosurface", bruker man absolutte posisjoner eller relative posisjoner?
    
-- TODO: Ta høyde for at ikke hele fieldet ikke vises samtidig? (Wrapping)
tileToScreenCoordinate :: Field -> Tile -> Position
tileToScreenCoordinate field tile
  | not $ contains grid tilePos = error $ "Error: Tile position not in the field GRID, tx: " ++ show tx ++ ", ty: " ++ show ty ++ "gridSize: " ++ show s ++ "."
  | outOfBounds = error $ "Error: Tile coordinates out of bounds, x: " ++ show x ++ ", y: " ++ show y ++ ", fdx: " ++ show fdx ++ ", fdy: " ++ show fdy ++ ", fx: " ++ show fx ++ ", fy: " ++ show fy ++ "."
  | tdx /= tdy = error "Error: X and Y tile dimensions differ, not a 'perfect' hexagon."
  | otherwise = (x,y)
  where
    tilePos@(tx, ty) = tPosition tile
    fieldPos@(fx, fy) = fFieldPosition field
    fieldDim@(fdx, fdy) = fFieldDimensions field
    tileDim@(tdx, tdy) = fTileDimensions field
    
    grid = fGrid field
    s = G.size grid
    
    -- absolute positions, using integer division - assumes even tile dimensions.
    halftdx = div tdx 2
    
    -- straight forward. 
    x = (s - 1 + tx)*tdx + ty*halftdx + fx
    
    -- find how much Y grows by for each row
    yGrowth = fromIntegral tdy - (fromIntegral tdy / (2.0 * sqrt 3))

    y = round (fromIntegral (s-1-ty) * yGrowth) + fy
    
    

    outOfBounds = 
      x + tdx > (fx + fdx) || 
      y > (fy + fdy) ||
      x < fx ||
      y < fy


-- returns a field with un-illuminated tiles filtered out.
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
lightRadius :: Position -> HexHexGrid -> Int -> M.Map Position Tile ->  [Position]
lightRadius origin grid strength  tileMap =
  lightRadii [] [origin] grid strength fieldSize tileMap
  where
    fieldSize = G.size grid
    
-- Expands to neighbours for each strength of original lightSource.
-- Future: Handle lightsources encountered here? cheaper perhaps
lightRadii :: [Position] -> [Position] -> HexHexGrid -> Int -> Int -> M.Map Position Tile -> [Position]
lightRadii pP                nP            _     1        _       _        = L.union pP nP
lightRadii previousPositions nextPositions grid strength fieldSize tileMap
  | L.null nonOpaque = L.union previousPositions nextPositions
  | otherwise = 
    lightRadii (nextPositions ++ previousPositions) unvisited grid (strength-1) fieldSize tileMap
  where
    nonOpaque = L.filter (\p -> case M.lookup p tileMap of
                             Just (Tile {tLightMask = Opaque }) -> False
                             _ -> True
                         ) nextPositions
                
                
    neighbourTiles = L.foldl' (\ps p -> L.union ps $ neighbours grid p) [] nonOpaque
    unvisited = neighbourTiles L.\\ previousPositions
