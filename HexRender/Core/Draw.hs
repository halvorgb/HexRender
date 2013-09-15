module HexRender.Core.Draw (drawField) where

import HexRender.Core.Model
import HexRender.Core.Assets
import HexRender.Core.Grid
import HexRender.Core.Draw.Primitive
import HexRender.Core.Draw.Image


import Control.Monad
import Graphics.UI.SDL as SDL
import Data.Map as M

-- Takes a hexstate (a field and an assetmap), returns a possibly updated assetmap.
-- Draws everythaangg.
drawField :: HexState -> IO AssetMap
drawField s@(f, m) = do
  
  -- filter out the tiles and objects that are actually shown.  
  s' <- foldM (\mp fn -> fn (f', mp)) m [drawBG, drawTiles, drawBorders, drawObjects]
  SDL.flip fieldSurface
  return s'
  
  where
    fieldSurface = fFieldSurface f
    f' = filterIllumination f
    
  
  
-------------------------------------------------------------
drawTiles :: HexState -> IO AssetMap
drawTiles (f, m) = foldM (\m' -> drawTile (f, m')) m $ M.elems $ fTiles f
  
drawObjects :: HexState -> IO AssetMap
drawObjects (f, m) = foldM (\m' -> drawObject (f, m')) m $ concat $ M.elems $ fObjects f

-- draws background, always first.
-- Force stretch? psht.
drawBG :: HexState -> IO AssetMap
drawBG (f, m) =
  case bg of
    Primitive (r, g, b, _) -> do
      fillSurface fieldSurf p d r g b -- undefined
      return m
      
    NonPrimitive {npKeyable = asset} -> do
      (surface, m') <- getAsset asset m
      drawImage fieldSurf surface p d
      return m'
  where
    fieldSurf = fFieldSurface f
    bg = fBackground f
    d = fFieldDimensions f
    p = fFieldPosition f

drawBorders :: HexState -> IO AssetMap
drawBorders s@(f, m) =
  case border of
    NoBorder -> 
      return m
    TileBorder thickness (r, g, b, a) -> do
      -- mapM_ drawHexagons@ tiles
      return m
--  mapM_
  where
    border = fTileBorder f
    tiles = fTiles f
    
{-m 
For alle tiles i [tiles], tegn et hexagon!
-- Unngår borders på tomme tiles, desirable?
-}

----------------------------------------------------------------
drawObject :: HexState -> Object -> IO AssetMap
drawObject s@(f, m) o = drawSprite s sprite position dimensions
  where
    sprite = oSprite o
    position = tileToScreenCoordinate f $ oPosition o
    dimensions = fTileDimensions f
    
    -- more calculations with offset and scale etc.


drawTile :: HexState -> Tile -> IO AssetMap 
drawTile s@(f, m) t = drawSprite s sprite position dimensions
  where
    sprite = tSprite t
    position = tileToScreenCoordinate f $ tPosition t
    dimensions = fTileDimensions f
    

--------------------------------------------------------------
-- position, dimensions are here in term of pixels, not grid positions.
drawSprite :: HexState -> Resource -> Position -> Dimensions -> IO AssetMap
drawSprite (f, m) r p d =
  case r of
    Primitive { } -> do
      drawHexagon fieldSurf r p d
      return m
      
    NonPrimitive {npKeyable = asset} -> do
      (surf, m') <- getAsset asset m
      drawImage fieldSurf surf p d
      return m'     
  where
    fieldSurf = fFieldSurface f