{-# LANGUAGE RecordWildCards, NumericUnderscores #-}
module SimIOSDL where

import Prelude
import Clash.Prelude hiding (lift)
import Interface

import RetroClash.Sim.SDL
import RetroClash.Sim.VGA
import RetroClash.Sim.VGASDL
import RetroClash.VGA

import Data.Word
import Data.Int
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc

import Control.Monad
import Control.Monad.State
import Data.Array.IO

import SDL (ticks)
import Control.Monad.Loops
import Control.Monad.Loops
import Text.Printf
import Control.Lens
import Data.Text

vgaMode = vga640x480at60

{-# INLINE runSDL #-}
runSDL :: Text -> (INPUT -> IO OUTPUT) -> IO ()
runSDL title runCycle = do
    buf <- newBufferArray

    let input = INPUT{ reset = False }

    let videoParams = MkVideoParams
            { windowTitle = title
            , screenScale = 1
            , screenRefreshRate = 60
            , reportFPS = True
            }

    t0 <- ticks
    flip evalStateT (initSink, (0, t0)) $ withMainWindow videoParams $ \events keyDown -> do
        when (keyDown ScancodeEscape) mzero

        untilM_ (return ()) $ do
            vgaOut <- do
                OUTPUT{..} <- liftIO $ runCycle input
                return (vgaHSYNC, vgaVSYNC, (vgaRED, vgaGREEN, vgaBLUE))
            zoom _1 $ lift $ vgaSinkBuf vgaMode buf vgaOut

        zoom _2 $ do
            (i, t0) <- get
            if i == 60 then do
                t <- ticks
                let dt = t - t0
                    fps = 1000 / (fromIntegral dt / 60) :: Double
                liftIO $ printf "60 frames in %d ms, %.1f fps\n" dt fps
                put (1, t)
              else put (i + 1, t0)

        return $ rasterizeBuffer buf
