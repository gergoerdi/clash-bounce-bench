{-# LANGUAGE RecordWildCards, NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
import Prelude
import Clash.Prelude hiding (undefined)
import FFI

import RetroClash.Sim.VGA
import RetroClash.Sim.SDL
import RetroClash.VGA

import Data.Word
import Data.Int
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc

import Control.Monad
import Control.Monad.State
import Data.Array.IO

import System.Clock
import Control.Monad.Loops
import Text.Printf

vgaMode = vga640x480at60

vgaSinkBuf
    :: (KnownNat w, KnownNat h, MonadIO m)
    => VGATimings ps w h
    -> BufferArray w h
    -> (Bit, Bit, (Word8, Word8, Word8))
    -> StateT (SinkState, SinkState) m Bool
vgaSinkBuf vgaMode (BufferArray arr) = vgaSink vgaMode writeBuf
  where
    writeBuf x y (r, g, b) = liftIO $ do
        writeArray arr (x, y, 0) r
        writeArray arr (x, y, 1) g
        writeArray arr (x, y, 2) b

millisec :: TimeSpec -> Int64
millisec (TimeSpec sec nsec) = sec * 1_000 + nsec `div` 1_000_000

main :: IO ()
main = do
    inp <- malloc
    poke inp $ INPUT{ reset = False }
    outp <- malloc

    buf <- newBufferArray
    flip evalStateT initSink $ withMainWindow "VGA" 1 $ \events keyState -> fmap Just $ do
        t0 <- liftIO $ getTime Monotonic
        untilM_ (return ()) $ do
            vgaOut <- do
                OUTPUT{..} <- liftIO $ do
                    topEntity inp outp
                    peek outp
                let hsync = vgaHSYNC
                    vsync = vgaVSYNC
                    vgaR = 0 -- fromIntegral vgaRED
                    vgaG = 0 -- fromIntegral vgaGREEN
                    vgaB = 0 -- fromIntegral vgaBLUE

                return (hsync, vsync, (vgaR, vgaG, vgaB))
            vgaSinkBuf vgaMode buf vgaOut
        t <- liftIO $ getTime Monotonic
        liftIO $ print $ millisec t - millisec t0

        return $ rasterizeBuffer buf
