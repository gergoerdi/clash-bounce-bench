{-# LANGUAGE RecordWildCards, NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}

import Prelude
import Clash.Prelude
import FFI

import Data.Word
import Data.Int
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Control.Monad
import System.Clock
import Control.Monad.Extra
import Text.Printf

millisec :: TimeSpec -> Int64
millisec (TimeSpec sec nsec) = sec * 1_000 + nsec `div` 1_000_000

main :: IO ()
main = alloca $ \inp -> alloca $ \outp -> do
    poke inp $ INPUT{ reset = False }

    let loop1 :: Int -> IO Int
        loop1 !n = do
            topEntity inp outp
            out@OUTPUT{..} <- peek outp
            let n' = n + 1
            if vgaHSYNC == False && vgaVSYNC == False then loop2 n' else loop1 n'

        loop2 :: Int -> IO Int
        loop2 !n = do
            topEntity inp outp
            out <- peek outp
            let n' = n + 1
            if vgaDE out then return n' else loop2 n'

        loop :: Int -> Int -> IO Int
        loop !k !n
          | k > 0 = loop (k - 1) =<< loop1 n
          | otherwise = return n

    t0 <- getTime Monotonic
    n <- loop 10 0
    t <- getTime Monotonic
    printf "Hand-translated C, from Haskell: %d cycles, %d ms\n" n (millisec t - millisec t0)
