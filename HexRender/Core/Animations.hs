module HexRender.Core.Animations (animateField) where

import HexRender.Core.Grid
import HexRender.Core.Model
import HexRender.Core.Draw

import Control.Concurrent (threadDelay)
import Control.Monad (foldM)


animateField :: HexState -> IO AssetMap
animateField s@(f, m) =
  if null anims
  then return m
  else do
    drawAnimations s
  where
    anims = fAnimations f



drawAnimations :: HexState -> IO AssetMap
drawAnimations s@(f,m) = do
  m' <- drawField (f,m)
  if null nonStop
  then return m'
  else do
    let s' = (f,m') 
    m'' <- foldM (drawAnim s') m' lowestSubtracted
    -- wait for lowestTime msecs.
    putStrLn "Delaying"

    threadDelay lowestTime
    drawAnimations (f',m'')
  where
    anims = fAnimations f
    anims' = map (\a ->
                   if aTime a == 0
                   then aNextFrame a
                   else a) anims
    
    
    nonStop = filter (/= Stop) anims'
    lowestTime = foldl (\a a' -> min a $ aTime a') (aTime $ head nonStop) $ tail nonStop
                 
    lowestSubtracted = map (\a -> a {aTime = (aTime a) - lowestTime}) nonStop
    
    f' = f {fAnimations = lowestSubtracted}
  

drawAnim :: HexState -> AssetMap -> Animation -> IO AssetMap
drawAnim s@(f,_) m a = do
  m' <- maybe (return m) (drawAnimSprite s) $ aSprite a
  let s' = (f, m')
  maybe (return m') (drawAnimText s') $ aText a
  
drawAnimSprite :: HexState -> AnimationSprite -> IO AssetMap
drawAnimSprite s@(f,m) (r, pos) = do
  putStrLn $ "Drawing sprite: " ++ show sprite
  drawSprite s sprite position dimensions
  where
    sprite = r
    position = tileToScreenCoordinate f $ pos
    dimensions = fTileDimensions f
  


drawAnimText :: HexState -> AnimationText -> IO AssetMap
drawAnimText s@(f,m) (r, pos, string) = return m -- TODO
