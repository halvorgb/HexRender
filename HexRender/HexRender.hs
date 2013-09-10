module HexRender.HexRender (render, shutdown) where

import HexRender.Core.Model
import HexRender.Core.Assets
import HexRender.Core.Draw

import Graphics.UI.SDL as SDL


-- Update the Field, render changes.
render :: HexState -> IO HexState
render s@(f, _) = do
  m' <- drawField s
  return (f, m')
  
-- unload resources.
shutdown :: HexState -> IO ()
shutdown = freeField 
