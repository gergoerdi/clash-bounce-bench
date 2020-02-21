{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bounce
import RetroClash.Sim.VGA
import RetroClash.Sim.SDL

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

-- vgaMode = vga640x480sim
vgaMode = vga150x150sim

vgaWidth :: (KnownNat w) => VGATimings ps w h -> SNat w
vgaWidth _ = SNat

vgaHeight :: (KnownNat h) => VGATimings ps w h -> SNat h
vgaHeight _ = SNat

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

main :: IO ()
main = do
    buf <- newBufferArray

    inChan <- liftIO newChan
    liftIO $ writeChan inChan ()
    is <- liftIO $ getChanContents inChan

    flip evalStateT (simulate topEntity' is, initSink) $ withMainWindow "VGA" 3 $ \events keyState -> fmap Just $ do
        i <- return ()

        t0 <- liftIO $ getTime Monotonic
        untilM_ (return ()) $ do
            vgaOut <- zoom _1 $ do
                (o:os) <- get
                _ <- liftIO $ readChan inChan
                liftIO $ writeChan inChan i
                put os
                return o

            zoom _2 $ vgaSinkBuf vgaMode buf vgaOut
        t <- liftIO $ getTime Monotonic
        liftIO $ print $ millisec t - millisec t0

        return $ rasterizeBuffer buf
  where
    topEntity' sw =
        let VGAOut{ vgaSync = VGASync{..}, ..} = topEntity clockGen resetGen sw
        in bundle (vgaHSync, vgaVSync, bundle (vgaR, vgaG, vgaB))

    millisec (TimeSpec sec nsec) = sec * 1_000 + nsec `div` 1_000_000
