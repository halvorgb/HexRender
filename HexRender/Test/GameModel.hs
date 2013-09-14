module HexRender.Test.GameModel where

import HexRender.Core.Model

data Character = Character { cObject :: Object }

type GameState = (HexState, Character)