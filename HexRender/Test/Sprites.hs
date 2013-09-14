module HexRender.Test.Sprites (grassTile, waterTile, lavaTile, rockTile, testCharacterObject) where

import HexRender.Core.Model

grassTile = Tile grass undefined Transparent
waterTile = Tile water undefined Transparent
lavaTile = Tile lava undefined   Transparent
rockTile = Tile rock undefined Opaque

grass = NonPrimitive { npKeyable = Image "./HexRender/Test/Tiles/grass_tile.png" }
water = NonPrimitive { npKeyable = Image "./HexRender/Test/Tiles/water_tile.png" }
lava = NonPrimitive { npKeyable = Image "./HexRender/Test/Tiles/lava_tile.png" }
rock = NonPrimitive { npKeyable = Image "./HexRender/Test/Tiles/rock_tile.png" }


testCharacterObject = Object { oSprite = testCharacter,
                               oPosition = (0,0),
                               oOffset = (0,0),
                               oScale = 1.0,
                               oZLevel = One,
                               oOrientation = Up,
                               oLightMask = LightSource 5 }
                      
                      
testCharacter = NonPrimitive { npKeyable = Image "./HexRender/Test/Sprites/dude.png" }
