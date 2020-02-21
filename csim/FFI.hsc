{-# LANGUAGE RecordWildCards, ForeignFunctionInterface #-}
module FFI where

import Prelude
import Clash.Prelude

import Data.Word
import Data.Int
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc

#include "Bounce.h"

data INPUT = INPUT
    { reset :: Bool
    }

data OUTPUT = OUTPUT
    { vgaHSYNC, vgaVSYNC :: Bit
    , vgaDE :: Bool
    , vgaRED, vgaGREEN, vgaBLUE :: Word64
    }
    deriving (Show)

foreign import ccall "Bounce" topEntity :: Ptr INPUT -> Ptr OUTPUT -> IO ()

instance Storable Bit where
    alignment = alignment . bitToBool
    sizeOf = sizeOf . bitToBool
    peek = fmap boolToBit . peek . castPtr
    poke ptr = poke (castPtr ptr) . bitToBool

instance Storable INPUT where
    alignment _ = #alignment INPUT
    sizeOf _ = #size INPUT
    peek ptr    = INPUT
        <$> (#peek INPUT, RESET) ptr
    poke ptr INPUT{..} = do
        (#poke INPUT, RESET) ptr reset

instance Storable OUTPUT where
    alignment _ = #alignment OUTPUT
    sizeOf _ = #size OUTPUT
    peek ptr    = OUTPUT
        <$> (#peek OUTPUT, VGA_HSYNC) ptr
        <*> (#peek OUTPUT, VGA_VSYNC) ptr
        <*> (#peek OUTPUT, VGA_DE)    ptr
        <*> (#peek OUTPUT, VGA_RED)   ptr
        <*> (#peek OUTPUT, VGA_GREEN) ptr
        <*> (#peek OUTPUT, VGA_BLUE)  ptr
    poke ptr OUTPUT{..} = do
        (#poke OUTPUT, VGA_HSYNC) ptr vgaHSYNC
        (#poke OUTPUT, VGA_VSYNC) ptr vgaVSYNC
        (#poke OUTPUT, VGA_DE)    ptr vgaDE
        (#poke OUTPUT, VGA_RED)   ptr vgaRED
        (#poke OUTPUT, VGA_GREEN) ptr vgaGREEN
        (#poke OUTPUT, VGA_BLUE)  ptr vgaBLUE
