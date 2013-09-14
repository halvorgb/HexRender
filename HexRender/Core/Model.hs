module HexRender.Core.Model where

import Graphics.UI.SDL as SDL
import qualified Data.Map as M

import Math.Geometry.Grid.Hexagonal2

type Position = (Int, Int)
type Offset = (Int, Int)
type Dimensions = (Int, Int)
type RGBA = (Int, Int, Int, Int)
type LightRadius = Int
type ResourcePath = String

type HexState = (Field, AssetMap)
                
type AssetMap = M.Map AssetKey Asset
type AssetKey = String
data AssetKeyable = Image ResourcePath | Sound ResourcePath
                  deriving (Eq, Show)

data Asset = ImageAsset SDL.Surface | SoundAsset Int -- temporary PLACEholDER int
           deriving(Eq)


data ZLevel = Zero | One | Two | Three | Four
            deriving(Bounded, Eq)

data Resource = Primitive RGBA | NonPrimitive { npKeyable :: AssetKeyable }
            deriving (Eq, Show)

data LightMask = Opaque | Transparent | LightSource LightRadius
               deriving (Eq)
                        
                          
data Direction = Up | Down | UpRight | UpLeft | DownRight | DownLeft
               deriving(Eq)


data TileBorder = NoBorder | 
                  TileBorder { tbThickness :: Int, 
                               tbColor :: RGBA                             
                             } deriving (Eq)

data Tile = Tile { tSprite :: Resource,
                   tPosition :: Position, 
                   tLightMask :: LightMask
                 }
            
data Object = Object { oSprite :: Resource,
                       oPosition :: Position,
                       oOffset :: Offset,
                       oScale :: Double,
                       oZLevel :: ZLevel,
                       oOrientation :: Direction,
                       oLightMask :: LightMask
                     }
            deriving (Eq)
                   
-- Field is a collection of tiles, objects and lightsources. Everything that is rendered.
data Field = Field { fFieldDimensions :: Dimensions,
                     fFieldSurface :: SDL.Surface,
                     fFieldPosition :: Position,
                     fTileDimensions :: Dimensions,
                     fTiles :: M.Map Position Tile,
                     fObjects :: M.Map Position Object,
                     fGrid :: RectHexGrid,
                     fBackground :: Resource,
                     fTileBorder :: TileBorder
                   }
                     
