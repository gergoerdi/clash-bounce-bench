{-# LANGUAGE NumericUnderscores, RecordWildCards, OverloadedStrings #-}
module Main where

import Bounce
import RetroClash.Sim.VGA
import RetroClash.Sim.SDL
import RetroClash.Sim.VGASDL
import RetroClash.Sim.IO

import RetroClash.Utils
import RetroClash.VGA
import Control.Monad.State
import Control.Monad.Loops
import Data.Foldable
import Control.Lens hiding (Index)
import Data.Array.IO
import Control.Concurrent

import Clash.Prelude hiding (lift)
import Data.Word
import Debug.Trace
import SDL hiding (get)
import Control.Lens
import System.Clock

vgaMode = vga640x480at60
-- vgaMode = vga150x150sim

main :: IO ()
main = do
    buf <- newBufferArray

    sim <- driveIO (simulate topEntity') undefined
    flip evalStateT initSink $ withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        t0 <- liftIO $ getTime Monotonic

        untilM_ (return ()) $ sim $ \vgaOut -> do
            frameFinished <- lift $ vgaSinkBuf vgaMode buf vgaOut
            return ((), frameFinished)

        t <- liftIO $ getTime Monotonic
        liftIO $ print $ millisec t - millisec t0

        return $ rasterizeBuffer buf
  where
    topEntity' sw =
        let VGAOut{ vgaSync = VGASync{..}, ..} = topEntity clockGen resetGen sw
        in bundle (vgaHSync, vgaVSync, bitCoerce <$> bundle (vgaR, vgaG, vgaB))

    millisec (TimeSpec sec nsec) = sec * 1_000 + nsec `div` 1_000_000

    videoParams = MkVideoParams
        { windowTitle = "VGA"
        , screenScale = 2
        , screenRefreshRate = 60
        }
