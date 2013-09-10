module HexRender.Test.Tiles (grassTile, waterTile, lavaTile, rockTile) where

import HexRender.Core.Model

grassTile = Tile grass undefined Transparent
waterTile = Tile water undefined Transparent
lavaTile = Tile lava undefined (LightSource 3)
rockTile = Tile rock undefined Opaque

grass = NonPrimitive { npKeyable = Image "./HexRender/Test/grass.png" }
water = NonPrimitive { npKeyable = Image "./HexRender/Test/water.png" }
lava = NonPrimitive { npKeyable = Image "./HexRender/Test/lava.png" }
rock = NonPrimitive { npKeyable = Image "./HexRender/Test/rock.png" }
