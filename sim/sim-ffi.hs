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

main :: IO ()
main = do
    inp <- malloc
    poke inp $ INPUT{ reset = False }
    outp <- malloc

    forM_ [1..60] $ \_ -> do
        let loop = do
                topEntity inp outp
                out <- peek outp
                if vgaVSYNC out == high then loop else return ()
        loop

        out <- peek outp
        print out

        let loop = do
                topEntity inp outp
                out <- peek outp
                if vgaVSYNC out == low then loop else return ()
        loop
