{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bounce
import RetroClash.Sim.VGA
import RetroClash.Sim.SDL
import RetroClash.Sim.VGASDL

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

    inChan <- newChan
    writeChan inChan ()
    is <- getChanContents inChan

    flip evalStateT (simulate topEntity' is, initSink) $ withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        i <- return ()

        t0 <- liftIO $ getTime Monotonic
        untilM_ (return ()) $ do
            vgaOut <- lift $ zoom _1 $ do
                (o:os) <- get
                _ <- liftIO $ readChan inChan
                liftIO $ writeChan inChan i
                put os
                return o

            lift $ zoom _2 $ vgaSinkBuf vgaMode buf vgaOut
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
