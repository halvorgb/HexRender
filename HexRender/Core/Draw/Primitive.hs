module HexRender.Core.Draw.Primitive (drawHexagon, fillSurface) where

import HexRender.Core.Model

import Graphics.UI.SDL as SDL

-- draws a right hexagon onto the screen
drawHexagon :: SDL.Surface -> Resource -> Position -> Dimensions -> IO ()
drawHexagon targetSurf r = case r of
  Primitive color -> error "Unimplemented drawHexagon."
  _ -> error "Attempted to draw non-primitive as primitive."

fillSurface = undefined