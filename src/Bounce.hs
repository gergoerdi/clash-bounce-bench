{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Bounce where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Clock
import Data.Maybe
import Data.Word
import Data.Proxy

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

vga150x150sim :: VGATimings ps 150 150
vga150x150sim = VGATimings
    { vgaHorizTiming = VGATiming Low (SNat @1) (SNat @1) (SNat @1)
    , vgaVertTiming  = VGATiming Low (SNat @1) (SNat @1) (SNat @1)
    }

vga640x480sim :: VGATimings ps 640 480
vga640x480sim = VGATimings
    { vgaHorizTiming = VGATiming Low (SNat @1) (SNat @1) (SNat @1)
    , vgaVertTiming  = VGATiming Low (SNat @1) (SNat @1) (SNat @1)
    }

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET" ::: Reset Dom25
    -> "DUMMY" ::: Signal Dom25 ()
    -> "VGA" ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board _ = vgaOut vgaSync $ bouncingBall vgaX vgaY
      where
        VGADriver{..} = vgaDriver vga640x480at60

mask
    :: (HiddenClockResetEnable dom)
    => Signal dom a
    -> (Signal dom (Maybe x) -> Signal dom (Maybe y) -> Signal dom a)
    -> Signal dom (Maybe x) -> Signal dom (Maybe y) -> Signal dom a
mask def f x y = mux visible (f x y) def
  where
    visible = (isJust <$> x) .&&. (isJust <$> y)

type BallSize = 35

bouncingBall
    :: (KnownNat w, KnownNat h, KnownNat r, KnownNat g, KnownNat b)
    => ((BallSize + 2) <= w, (BallSize + 1) <= h)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Index w))
    -> Signal dom (Maybe (Index h))
    -> Signal dom (Unsigned r, Unsigned g, Unsigned b)
bouncingBall vgaX vgaY = mux isBall ballColor backColor
  where
    frameEnd = isFalling False (isJust <$> vgaY)

    (ballX, speedX) = unbundle $ regEn (0, 3) frameEnd $
        bounceBetween (0, leftWall) <$> bundle (ballX, speedX)
    (ballY, speedY) = unbundle $ regEn (0, 2) frameEnd $
        bounceBetween (0, bottomWall) <$> bundle (ballY, speedY)

    maxOf :: forall n p. (KnownNat n, 1 <= n) => p (Maybe (Index n)) -> Signed (CLog 2 n + 1)
    maxOf _ = fromIntegral (maxBound :: Index n)

    leftWall = maxOf vgaX - ballSize
    bottomWall = maxOf vgaY - ballSize

    ballSize :: (Num a) => a
    ballSize = snatToNum (SNat @BallSize)

    isBall = (near <$> ballX <*> vgaX) .&&. (near <$> ballY <*> vgaY)
      where
        near x0 = maybe False $ \x -> let x' = fromIntegral x in x0 <= x' && x' < (x0 + ballSize)

    ballColor = pure (0xf0, 0xe0, 0x40)
    backColor = pure (0x30, 0x30, 0x30)

bounceBetween (lo, hi) = reflect (lo, 1) . reflect (hi, -1) . move

move :: (Num a) => (a, a) -> (a, a)
move (x, dx) = (x + dx, dx)

reflect :: (Num a, Num a', Ord a, Ord a') => (a, a') -> (a, a') -> (a, a')
reflect (p, n) (x, dx)
    | sameDirection n diff = (p + diff, negate dx)
    | otherwise = (x, dx)
  where
    sameDirection u v = compare 0 u == compare 0 v
    diff = p - x

makeTopEntity 'topEntity
