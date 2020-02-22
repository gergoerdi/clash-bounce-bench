{-# LANGUAGE RecordWildCards, NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
import Prelude
import Clash.Prelude hiding (undefined)
import FFI

import RetroClash.Sim.SDL
import RetroClash.Sim.VGA
import RetroClash.Sim.VGASDL
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
import Control.Lens

vgaMode = vga640x480at60

millisec :: TimeSpec -> Int64
millisec (TimeSpec sec nsec) = sec * 1_000 + nsec `div` 1_000_000

main :: IO ()
main = do
    inp <- malloc
    poke inp $ INPUT{ reset = False }
    outp <- malloc

    buf <- newBufferArray
    flip evalStateT (0, initSink) $ withMainWindow "VGA" 1 $ \events keyState -> fmap Just $ do
        t0 <- liftIO $ getTime Monotonic
        untilM_ (return ()) $ do
            vgaOut <- do
                OUTPUT{..} <- liftIO $ do
                    topEntity inp outp
                    peek outp
                let hsync = vgaHSYNC
                    vsync = vgaVSYNC
                    vgaR = fromIntegral vgaRED
                    vgaG = fromIntegral vgaGREEN
                    vgaB = fromIntegral vgaBLUE

                return (hsync, vsync, (vgaR, vgaG, vgaB))
            zoom _2 $ vgaSinkBuf vgaMode buf vgaOut

        zoom _1 $ do
            i <- get
            if i == 60 then do
                t <- liftIO $ getTime Monotonic
                liftIO $ print $ millisec t - millisec t0
                put 0
              else put $! (i + 1)

        return $ rasterizeBuffer buf
