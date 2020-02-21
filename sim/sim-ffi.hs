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

    t0 <- getTime Monotonic
    forM_ [1..60] $ \_ -> do
        whileM $ do
            topEntity inp outp
            out <- peek outp
            return $ not $ finished out
        whileM $ do
            topEntity inp outp
            out <- peek outp
            return $ finished out
    t <- getTime Monotonic
    printf "%d ms\n" (millisec t - millisec t0)
