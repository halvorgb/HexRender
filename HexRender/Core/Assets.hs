module HexRender.Core.Assets (getAsset, freeField) where

import HexRender.Core.Model

import qualified Data.Map as M
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDL_image
import Graphics.UI.SDL.Primitives as SDL_primitives

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
  mapM_ (freeResource m . tSprite . snd) $ M.toList $ fTiles f
  mapM_ (freeResource m . oSprite . snd) $ M.toList $ fObjects f
  freeResource m $ fBackground f

  

freeResource ::  AssetMap -> Resource -> IO ()
freeResource m r =
  case r of
    NonPrimitive { } ->  case M.lookup k m of
      Just a -> unloadAsset a
      _ -> return ()
    _ -> return ()
  where
    k = show r
    

-- unloads/destroys said assets.
unloadAsset :: Asset -> IO ()
unloadAsset a = case a of
  ImageAsset surf -> unloadImage surf
  _ -> error "unimplemented unload functionality"

-- loads an asset.
loadAsset ::  AssetKeyable -> IO Asset
loadAsset r = case r of
  Image path -> loadImage path
  _ -> error "unimplemented load functionality"





-- loads an image
loadImage :: ResourcePath -> IO Asset
loadImage path = do
  surf <- SDL_image.load path
  return (ImageAsset surf)
  
-- frees an image.
unloadImage :: SDL.Surface -> IO ()
unloadImage = freeSurface
