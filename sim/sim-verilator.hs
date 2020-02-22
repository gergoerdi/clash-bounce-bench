{-# LANGUAGE RecordWildCards, ForeignFunctionInterface #-}
import Prelude
import Clash.Prelude hiding (undefined)
import Interface
import SimIO

import Foreign.Storable
import Foreign.Marshal.Alloc

foreign import ccall unsafe "hello" chello :: IO ()

{-# INLINE withRunner #-}
withRunner :: ((INPUT -> IO OUTPUT) -> IO a) -> IO a
-- withRunner act = alloca $ \inp -> alloca $ \outp -> act $ \input -> do
--     poke inp input
--     topEntity inp outp
--     peek outp
withRunner act = do
    chello
    act undefined

main :: IO ()
main = withRunner $ \runCycle -> do
    putStrLn "Hello"
