{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module RetroClash.Sim.VGA
    ( vgaSink
    , initSink
    , SinkState
    ) where

import RetroClash.Utils
import RetroClash.VGA

import Control.Monad.State
import Data.Foldable (for_)
import Control.Lens hiding (Index)
import Clash.Prelude hiding (lift)
import Data.Proxy

import Debug.Trace

vgaRetrace :: VGATiming visible -> (Int, Bool)
vgaRetrace VGATiming{..} = (snatToNum pulseWidth + snatToNum postWidth - 1, toActiveDyn polarity True == high)

data SinkState
    = Visible Int
    | WaitSync Bool
    | Retrace Int
    deriving (Show)

{-# INLINE vgaSink #-}
vgaSink
    :: forall w h r g b m ps. (KnownNat w, KnownNat h, Monad m)
    => VGATimings ps w h
    -> (Int -> Int -> (r, g, b) -> m ())
    -> (Bool, Bool, (r, g, b))
    -> StateT (SinkState, SinkState) m Bool
vgaSink VGATimings{..} paint (hsync0, vsync0, color) = do
    (x, endLine) <- zoom _1 $ direction w horizRetrace hsync
    (y, endFrame) <- zoom _2 $ (if endLine then id else undo) $ direction h vertRetrace vsync
    for_ (liftA2 (,) x y) $ \(x, y) -> lift $ paint x y color
    return $ endLine && endFrame
  where
    (horizRetrace, hsyncTarget) = vgaRetrace vgaHorizTiming
    (vertRetrace, vsyncTarget) = vgaRetrace vgaVertTiming

    vsync = vsync0 == vsyncTarget
    hsync = hsync0 == hsyncTarget

    w = fromIntegral (maxBound :: Index w)
    h = fromIntegral (maxBound :: Index h)

    undo act = do
        s <- get
        x <- act
        put s
        return x

    -- direction :: (KnownNat vis, Monad m) => Int -> Bool -> StateT (SinkState vis) m (Maybe (Index vis), Bool)
    direction vis retrace sync = do
        s <- get
        case s of
            Retrace n -> do
                put $ let n' = n + 1 in if n' == retrace then Visible 0 else Retrace n'
                return (Nothing, False)
            Visible i -> do
                put $ if i == vis then WaitSync sync else Visible (i + 1)
                return (Just i, False)
            WaitSync b -> do
                let end = not b && sync
                put $ if end then Retrace 0 else WaitSync sync
                return (Nothing, end)

initSink :: (SinkState, SinkState)
initSink = (WaitSync False, WaitSync False)
