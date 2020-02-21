{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}

import Prelude
import Data.Word
import Data.Int
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Control.Monad

#include "Bounce.h"

data INPUT = INPUT
    { reset :: Bool
    }

data OUTPUT = OUTPUT
    { vgaHSYNC, vgaVSYNC, vgaDE :: Bool
    , vgaRED, vgaGREEN, vgaBLUE :: Word64
    }
    deriving (Show)

foreign import ccall "Bounce" topEntity :: Ptr INPUT -> Ptr OUTPUT -> IO ()

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

main :: IO ()
main = do
    inp <- malloc
    poke inp $ INPUT{ reset = False }
    outp <- malloc

    forM_ [1..60] $ \_ -> do
        let loop = do
                topEntity inp outp
                out <- peek outp
                if vgaVSYNC out then loop else return ()
        loop

        -- out <- peek outp
        -- print out

        let loop = do
                topEntity inp outp
                out <- peek outp
                if not (vgaVSYNC out) then loop else return ()
        loop
