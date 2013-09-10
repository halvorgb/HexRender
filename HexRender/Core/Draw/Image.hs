module HexRender.Core.Draw.Image (drawImage) where

import HexRender.Core.Model

import Graphics.UI.SDL as SDL

-- draws the image/surface onto the screen.
drawImage :: SDL.Surface -> Asset -> Position -> Dimensions -> IO ()
drawImage targetSurface asset (posx, posy) (dimx, dimy) = case asset of
  ImageAsset sourceSurface -> do
    let sourceRect = Just (SDL.Rect 0 0 dimx dimy)
    let targetRect   = Just (SDL.Rect posx posy 0 0)
    SDL.blitSurface sourceSurface sourceRect targetSurface targetRect
    return () -- ignore bool feedback!
  _ -> error "Attempted to draw non-image as image."
