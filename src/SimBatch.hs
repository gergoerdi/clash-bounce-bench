{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
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
    t0 <- getTime Monotonic
    let ins = L.repeat ()
        finished (hsync, vsync, _) = hsync == low && vsync == low
        outs = takeWhile (not . finished) $ simulate topEntity' ins
        len = L.length outs
    len `seq` return ()
    t <- getTime Monotonic
    printf "%d cycles, %d ms\n" len (millisec t - millisec t0)
  where
    topEntity' sw =
        let VGAOut{ vgaSync = VGASync{..}, ..} = topEntity clockGen resetGen sw
        in bundle (vgaHSync, vgaVSync, bundle (vgaR, vgaG, vgaB))

    millisec (TimeSpec sec nsec) = sec * 1_000 + nsec `div` 1_000_000
