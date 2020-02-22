{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module RetroClash.Sim.VGASDL
    ( vgaSinkBuf
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Sim.VGA
import RetroClash.Sim.SDL

import Control.Monad.State
import Data.Word
import Data.Array.IO

{-# INLINE vgaSinkBuf #-}
vgaSinkBuf
    :: (KnownNat w, KnownNat h, MonadIO m)
    => VGATimings ps w h
    -> BufferArray w h
    -> (Word8, Word8, (Word8, Word8, Word8))
    -> StateT (SinkState, SinkState) m Bool
vgaSinkBuf vgaMode (BufferArray arr) = vgaSink vgaMode writeBuf
  where
    writeBuf x y (r, g, b) = liftIO $ do
        writeArray arr (x, y, 0) r
        writeArray arr (x, y, 1) g
        writeArray arr (x, y, 2) b
