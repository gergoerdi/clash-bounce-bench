{-# LANGUAGE OverloadedStrings #-}
import Prelude
import Clash.Prelude hiding (undefined)
import Interface
import FFI
import SimIOSDL

import Foreign.Storable
import Foreign.Marshal.Alloc

{-# INLINE withRunner #-}
withRunner :: ((INPUT -> IO OUTPUT) -> IO a) -> IO a
withRunner act = alloca $ \inp -> alloca $ \outp -> act $ \input -> do
    poke inp input
    topEntity inp outp
    peek outp

main :: IO ()
main = withRunner $ runSDL "Hand-translated C"
