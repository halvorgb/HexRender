module HexRender.Test.GameModel where

import Data.Map as M

import HexRender.Core.Model



type GameState = (HexState, Game)


data Character = Character { cObject :: Object }
                 deriving (Eq)

data GameTileType = Water | Lava | Rock | Grass
                  deriving(Eq)

data GameTile = GameTile { gtTile :: Tile,
                           gtType :: GameTileType
                         }
                deriving (Eq)
                           

data Game = Game { gGameTiles :: M.Map Position GameTile,
                   gCharacter :: Character
                 }
