module HexRender.Core.Assets (getAsset, freeField) where

import HexRender.Core.Model

import Control.Monad
import qualified Data.Map as M
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDL_image
import Graphics.UI.SDL.Primitives as SDL_primitives
import Graphics.UI.SDL.TTF as TTF

-- gets an asset, if it does not exist - create it.
getAsset :: AssetKeyable -> AssetMap -> IO (Asset, AssetMap)
getAsset r m =
  case M.lookup k m of
    Just a ->
      return (a, m)
    Nothing -> do
      a <- loadAsset r 
      return (a, M.insert k a m)
  where
    k = show r

freeField :: HexState -> IO ()
freeField (f, m) = do
  m' <- M.foldrWithKey freeAsset (return m) m
  unless (M.null m')
    $ error "Error: Failed to free all values in the AssetMap"
  
  
freeAsset :: AssetKey -> Asset -> IO AssetMap -> IO AssetMap
freeAsset k a m = do
  unloadAsset a
  m' <- m
  return $ M.delete k m'


-- unloads/destroys said assets.
unloadAsset :: Asset -> IO ()
unloadAsset a = case a of
  ImageAsset surf -> unloadImage surf
  _ -> error "unimplemented unload functionality"

-- loads an asset.
loadAsset ::  AssetKeyable -> IO Asset
loadAsset r = case r of
  Image path -> loadImage path
  Font _ path size -> loadFont path size
  _ -> error "unimplemented load functionality"





-- loads an image
loadImage :: ResourcePath -> IO Asset
loadImage path = do
  surf <- SDL_image.load path
  return $ ImageAsset surf
  
-- frees an image.
unloadImage :: SDL.Surface -> IO ()
unloadImage = freeSurface


-- loads a font
loadFont :: ResourcePath -> Int -> IO Asset
loadFont path size = do 
  f <- TTF.openFont path size
  return $ FontAsset f

-- frees a font
unloadFont :: TTF.Font -> IO ()
unloadFont = closeFont