{-# LANGUAGE RecordWildCards, NumericUnderscores #-}

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
main = do
    inp <- malloc
    poke inp $ INPUT{ reset = False }
    outp <- malloc

    let finished OUTPUT{..} = vgaHSYNC == low && vgaVSYNC == low

    let loop1 n = do
            topEntity inp outp
            out <- peek outp
            let n' = n + 1
            if finished out then loop2 n' else loop1 n'
        loop2 n = do
            topEntity inp outp
            out <- peek outp
            let n' = n + 1
            if vgaDE out then return n' else loop2 n'

    t0 <- getTime Monotonic
    n <- loop1 (0 :: Int)
    t <- getTime Monotonic
    printf "%d cycles, %d ms\n" n (millisec t - millisec t0)
