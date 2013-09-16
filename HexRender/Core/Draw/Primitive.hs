module HexRender.Core.Draw.Primitive (drawHexagon, fillSurface) where

import HexRender.Core.Model

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Video as Video

import GHC.Word

-- draws a right hexagon onto the screen
drawHexagon :: SDL.Surface -> Resource -> Position -> Dimensions -> IO ()
drawHexagon targetSurf r = case r of
  Primitive color -> error "Unimplemented drawHexagon."
  _ -> error "Attempted to draw non-primitive as primitive."

fillSurface :: SDL.Surface -> Position -> Dimensions -> Int -> Int -> Int -> IO ()
fillSurface targetSurf p@(x,y) d@(w, h) r g b = do
  let targetRect = Just (SDL.Rect x y w h)
  pixel <- (mapRGB . surfaceGetPixelFormat) targetSurf r' g' b'
  SDL.fillRect targetSurf targetRect pixel
  return ()
  where
    r' = fromIntegral r :: Word8
    g' = fromIntegral g :: Word8
    b' = fromIntegral b :: Word8