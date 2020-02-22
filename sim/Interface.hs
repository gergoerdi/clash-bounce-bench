module Interface where

import Prelude
import Clash.Prelude

import Data.Word
import Data.Int

data INPUT = INPUT
    { reset :: Word8
    }
    deriving (Show)

data OUTPUT = OUTPUT
    { vgaHSYNC, vgaVSYNC :: Word8
    , vgaDE :: Word8
    , vgaRED, vgaGREEN, vgaBLUE :: Word8
    }
    deriving (Show)
