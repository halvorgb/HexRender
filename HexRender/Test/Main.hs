module HexRender.Test.Main(main) where

import Data.Map as M
import Data.List as L
import Data.Maybe
import System.Random
import Control.Monad.Trans.State
import Control.Monad

import Graphics.UI.SDL as SDL

import Math.Geometry.Grid as G

import HexRender.HexRender
import HexRender.Utilities
import HexRender.Core.Model as HexModel

import HexRender.Test.Helpers
import HexRender.Test.Data
import HexRender.Test.GameModel
import HexRender.Test.LevelGenerator


main :: IO ()
main = do
  SDL.init [ SDL.InitEverything ]
  SDL.enableUnicode True
  
  setVideoMode 1600 800 32 []
  
  setCaption "HexRender!" "hexRender."
  
  mainSurf <- SDL.getVideoSurface
  
  SDL.flip mainSurf
  
--  tiles <- randomTiles $ indices grid
--  let gtMap = M.fromList tiles
  gtMap <- randomLevel grid
  let (cPos,_) = fromJust $ L.find (\(_,gt) -> gtType gt /= Rock) $ M.toAscList gtMap
  let object = testCharacterObject { oPosition = cPos }
  let char = Character object
  let oMap = M.fromList [(cPos, [object])]
  let tMap = M.map gtTile gtMap
  gameLoop ((setupTestField mainSurf tMap oMap grid, M.empty), Game gtMap char)
  where
    grid = createGrid (11,21) (1008, 784) (64, 64)
--    object = (oPosition $ cObject character, [testCharacterObject])

--    character = Character testCharacterObject
    

gameLoop :: GameState -> IO ()
gameLoop s@(hs, g) = do
  hs' <- render hs
  waitEventBlocking >>= handleInput (hs', g)
  where
    c = gCharacter g
    
    
    
    
handleInput :: GameState -> Event -> IO ()
handleInput s@(hs, c) e = case e of
  Quit -> shutdownTest s
  (KeyDown (Keysym key _ char)) -> 
    case (key, char) of
      (SDLK_a,_) -> gameLoop $ handleMovement s DownLeft
      (SDLK_q,_) -> gameLoop $ handleMovement s UpLeft
      (SDLK_d,_) -> gameLoop $ handleMovement s DownRight
      (SDLK_e,_) -> gameLoop $ handleMovement s UpRight
      (SDLK_s,_) -> gameLoop $ handleMovement s Down
      (SDLK_w,_) -> gameLoop $ handleMovement s Up
      _ -> waitEventBlocking >>= handleInput s
  _ -> waitEventBlocking >>= handleInput s
  

handleMovement :: GameState -> HexModel.Direction -> GameState
handleMovement s@(_, g) dir =
  maybe s ( moveCharacter s) gt
  where
    c = gCharacter g
    gt = gameTileFromDirection (oPosition $ cObject c) dir g

moveCharacter :: GameState -> GameTile -> GameState
moveCharacter gs@((f,a), g) gt = 
  if gtType gt == Rock
  then gs
  else ((newField, a), g {gCharacter = Character newObject})
  where
    c = gCharacter g
    
    oldObject = cObject c
    oldCharacterPos = oPosition oldObject
    newCharacterPos = tPosition $ gtTile gt
    newObject = oldObject { oPosition = newCharacterPos}
    newField = f { fObjects = M.insertWith (++) newCharacterPos [newObject] $ 
                              
                              M.update (\os -> let os' = L.delete oldObject os
                                               in if L.null os'
                                                  then Nothing
                                                  else Just os') 
                              oldCharacterPos $ fObjects f }


shutdownTest :: GameState ->  IO ()
shutdownTest s@(hs, c) = do
  shutdown hs
  SDL.quit
      
  
  
  
  
  
  
  -- helper functions
setupTestField :: SDL.Surface -> M.Map Position Tile -> M.Map Position [Object]-> HexGrid ->  Field
setupTestField surf tiles objects grid  = Field { fFieldDimensions = (1024-32, 800-32),
                                                  fFieldSurface = surf,
                                                  fFieldPosition = (16, 16),
                                                  fTileDimensions = (64, 64),
                                                  fTiles = tiles,
                                                  fObjects = objects,
                                                  fGrid = grid,
                                                  fBackground = Primitive (15, 15, 15, 0),
                                                  fTileBorder = NoBorder,
                                                  fAnimations = [tardAnimation]
                                                }