{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module Main where

import Bounce
import RetroClash.Sim.VGA
import RetroClash.Sim.SDL

import RetroClash.Utils
import RetroClash.VGA
import Control.Monad.State
import Data.Foldable
import Control.Lens hiding (Index)
import Control.Concurrent

import Clash.Prelude hiding (lift)
import Data.Word
import Debug.Trace
import Data.List as L
import System.Clock

main :: IO ()
main = do
    t0 <- getTime Monotonic
    let loop ((hsync, vsync, _):os) = do
            let endOfFrame = hsync == low && vsync == low
            unless endOfFrame $ loop os
    loop $ simulate topEntity' $ L.repeat ()
    t <- getTime Monotonic
    print $ millisec t - millisec t0
  where
    topEntity' sw =
        let VGAOut{ vgaSync = VGASync{..}, ..} = topEntity clockGen resetGen sw
        in bundle (vgaHSync, vgaVSync, bundle (vgaR, vgaG, vgaB))

    millisec (TimeSpec sec nsec) = sec * 1_000 + nsec `div` 1_000_000
