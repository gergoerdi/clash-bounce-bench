{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Bounce

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
import Text.Printf

main :: IO ()
main = do
    let go1 !n ((hsync, vsync, _, _):os)
          | hsync == low && vsync == low = go2 (n + 1) os
          | otherwise = go1 (n + 1) os
        go2 !n ((hsync, vsync, de, _):os)
          | de = (n + 1, os)
          | otherwise = go2 (n + 1) os
        go !k !n os
          | k > 0 = uncurry (go (k - 1)) $ go1 n os
          | otherwise = n

    let ins = L.repeat ()
        outs = simulate topEntity' ins
        cycles = go (10 :: Int) (0 :: Int) outs

    t0 <- getTime Monotonic
    cycles `seq` return ()
    t <- getTime Monotonic

    printf "Clash: %d cycles, %d ms\n" cycles (millisec t - millisec t0)
  where
    topEntity' sw =
        let VGAOut{ vgaSync = VGASync{..}, ..} = topEntity clockGen resetGen sw
        in bundle (vgaHSync, vgaVSync, vgaDE, bundle (vgaR, vgaG, vgaB))

    millisec (TimeSpec sec nsec) = sec * 1_000 + nsec `div` 1_000_000
