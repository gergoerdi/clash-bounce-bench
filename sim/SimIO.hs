{-# LANGUAGE RecordWildCards, NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}
module SimIO where

import Prelude
import Clash.Prelude
import Interface

import Data.Int
import System.Clock
import Text.Printf

millisec :: TimeSpec -> Int64
millisec (TimeSpec sec nsec) = sec * 1_000 + nsec `div` 1_000_000

{-# INLINE runBench #-}
runBench :: (INPUT -> IO OUTPUT) -> IO ()
runBench runCycle = do
    let inp = INPUT{ reset = False }

    let loop1 :: Int -> IO Int
        loop1 !n = do
            out@OUTPUT{..} <- runCycle inp
            let n' = n + 1
            if vgaHSYNC == low && vgaVSYNC == low then loop2 n' else loop1 n'

        loop2 :: Int -> IO Int
        loop2 !n = do
            out@OUTPUT{..} <- runCycle inp
            let n' = n + 1
            if vgaDE then return n' else loop2 n'

        loop :: Int -> Int -> IO Int
        loop !k !n
          | k > 0 = loop (k - 1) =<< loop1 n
          | otherwise = return n

    t0 <- getTime Monotonic
    n <- loop 10 0
    t <- getTime Monotonic
    printf "Hand-translated C, from Haskell: %d cycles, %d ms\n" n (millisec t - millisec t0)
