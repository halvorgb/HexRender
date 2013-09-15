module HexRender.Test.Main(main) where

import Data.Map as M
import Data.List as L
import System.Random
import Control.Monad.Trans.State
import Control.Monad

import Graphics.UI.SDL as SDL

import Math.Geometry.Grid

import HexRender.HexRender
import HexRender.Utilities
import HexRender.Core.Model as HexModel


import HexRender.Test.Sprites
import HexRender.Test.GameModel



main :: IO ()
main = do
  SDL.init [ SDL.InitEverything ]
  SDL.enableUnicode True
  
  setVideoMode 1600 800 32 []
  
  setCaption "HexRender!" "hexRender."
  
  mainSurf <- SDL.getVideoSurface
  
  SDL.flip mainSurf
  
  tiles <- randomTiles $ (indices grid)
  
  gameLoop ((setupTestField mainSurf (M.fromList tiles) (M.fromList [object]) grid, M.empty), character)
  where
    grid = createGrid (11,22) (1008, 784) (64, 64)
    object = ((0,0), [testCharacterObject])

    character = Character testCharacterObject
    

gameLoop :: GameState -> IO ()
gameLoop s@(hs, c) = do
  hs' <- render hs
  waitEventBlocking >>= (handleInput (hs', c))
    
    
    
    
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
      _ -> waitEventBlocking >>= (handleInput s)
  _ -> waitEventBlocking >>= (handleInput s)
  

handleMovement :: GameState -> HexModel.Direction -> GameState
handleMovement s@((f, _), c) dir =
  maybe s ( moveCharacter s) t
  where
    t = tileFromDirection (oPosition $ cObject c) dir f 

moveCharacter :: GameState -> Tile -> GameState
moveCharacter gs@((f,a), c) t = 
  case t of
    Tile {tLightMask = Opaque} -> gs
    _ ->  ((newField, a), Character newObject)
  where
    
    oldObject = cObject c
    oldCharacterPos = oPosition oldObject
    newCharacterPos = tPosition t
    newObject = oldObject { oPosition = newCharacterPos}
    newField = f { fObjects = M.insertWith (\o os -> o ++os) newCharacterPos [newObject] $ 
                              
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
setupTestField surf tiles objects grid  = Field { fFieldDimensions = (1024, 800),
                                                  fFieldSurface = surf,
                                                  fFieldPosition = (16, 16),
                                                  fTileDimensions = (64, 64),
                                                  fTiles = tiles,
                                                  fObjects = objects,
                                                  fGrid = grid,
                                                  fBackground = Primitive (0, 0, 0, 0),
                                                  fTileBorder = NoBorder
                            }
                      
                      
randomTiles :: [Position] -> IO [(Position, Tile)]
randomTiles ps  = do
  g <- getStdGen
  let psts = zip ps $ fst $ rResources (length ps) g
  mapM createTile psts
  where
    createTile (pos,tile) = return (pos, tile { tPosition = pos })

rResources :: Int -> StdGen -> ([Tile], StdGen)
rResources n = runState (replicateM n (state rResource))

rResource :: StdGen -> (Tile, StdGen)
rResource g
  | r < 5 = (lavaTile, g')
  | r < 40 = (rockTile, g')
  | r < 65 = (waterTile, g')
  | otherwise = (grassTile, g')
  where
    (r, g') = randomR (0, 100) g :: (Int, StdGen)
