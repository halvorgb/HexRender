module HexRender.Core.Model where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import qualified Data.Map as M

import Math.Geometry.Grid.Hexagonal2

type Position = (Int, Int)
type Offset = (Int, Int)
type Dimensions = (Int, Int)
type RGBA = (Int, Int, Int, Int)
type LightRadius = Int
type ResourcePath = String

type HexState = (Field, AssetMap)
                
type HexGrid = RectHexGrid

type AssetMap = M.Map AssetKey Asset
type AssetKey = String
data AssetKeyable = Image ResourcePath | Sound ResourcePath | Font FontDesc ResourcePath Int
                  deriving (Eq, Show)

data Asset = ImageAsset SDL.Surface | SoundAsset Int  -- int placeHolder....
           | FontAsset TTF.Font
           deriving(Eq)


type FontDesc = String


data ZLevel = Zero | One | Two | Three | Four
            deriving(Bounded, Eq)

data Resource = Primitive RGBA | NonPrimitive { npKeyable :: AssetKeyable }
            deriving (Eq, Show)

data LightMask = Opaque | Transparent | LightSource LightRadius
               deriving (Eq)
                        
                          
data Direction = Up | Down | UpRight | UpLeft | DownRight | DownLeft
               deriving(Bounded, Eq, Show)


data TileBorder = NoBorder | 
                  TileBorder { tbThickness :: Int, 
                               tbColor :: RGBA                             
                             } deriving (Eq)

data Tile = Tile { tSprite :: Resource,
                   tPosition :: Position, 
                   tLightMask :: LightMask
                 }
          deriving(Eq)
            
type AnimationSprite = (Resource, Position);
type AnimationText =   (Resource, Position, String);

data Animation = Animation { aSprite    :: Maybe AnimationSprite,
                             aText      :: Maybe AnimationText,
                             aTime      :: Int,
                             aNextFrame :: Animation
                           }
               | Stop
               deriving(Eq)





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
                     fObjects :: M.Map Position [Object],
                     fGrid :: HexGrid,
                     fBackground :: Resource,
                     fTileBorder :: TileBorder,
                     fAnimations :: [Animation]

                   }