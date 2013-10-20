module HexRender.HexRender (render, shutdown) where

import HexRender.Core.Animations
import HexRender.Core.Model
import HexRender.Core.Assets
import HexRender.Core.Draw 
import qualified HexRender.Core.Grid as G

import Graphics.UI.SDL as SDL

import Math.Geometry.Grid.Hexagonal2


-- Update the Field, render changes.
render :: HexState -> IO HexState
render s@(f, _) = do
  --m' <- drawField s
  m'' <- animateField s --(f,m')
    
  return (f, m'')
  
-- unload resources.
shutdown :: HexState -> IO ()
shutdown = freeField 