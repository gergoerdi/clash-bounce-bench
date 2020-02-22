import Prelude
import Clash.Prelude
import Interface
import FFI
import SimIO

import Foreign.Storable
import Foreign.Marshal.Alloc

{-# INLINE withRunner #-}
withRunner :: ((INPUT -> IO OUTPUT) -> IO a) -> IO a
withRunner act = alloca $ \inp -> alloca $ \outp -> act $ \input -> do
    poke inp input
    topEntity inp outp
    peek outp

main :: IO ()
main = withRunner runBench
