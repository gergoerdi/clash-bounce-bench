{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TypeApplications #-}
module RetroClash.Sim.SDL
    ( withMainWindow
    , Rasterizer

    , BufferArray(..)
    , newBufferArray

    , rasterizePattern
    , rasterizeBuffer
    , rasterizeRay
    ) where

import Prelude
import Clash.Prelude hiding (lift)
import RetroClash.Utils

import SDL hiding (get)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Control.Monad
import Control.Monad.IO.Class
import Data.Array.IO
import Data.IORef

type Color = (Word8, Word8, Word8)
type Draw w h = (Index w, Index h) -> Color

screenRefreshRate :: Word32
screenRefreshRate = 60

newtype Rasterizer (w :: Nat) (h :: Nat) = Rasterizer{ runRasterizer :: Ptr Word8 -> Int -> IO () }

rasterizePattern :: (KnownNat w, KnownNat h) => Draw w h -> Rasterizer w h
rasterizePattern draw = Rasterizer $ \ptr stride -> do
    forM_ [minBound..maxBound] $ \y -> do
        let base = fromIntegral y * stride
        forM_ [minBound .. maxBound] $ \x -> do
            let offset = base + (fromIntegral x * 4)
            let (r, g, b) = draw (x, y)
            pokeElemOff ptr (offset + 0) maxBound
            pokeElemOff ptr (offset + 1) b
            pokeElemOff ptr (offset + 2) g
            pokeElemOff ptr (offset + 3) r

newtype BufferArray (w :: Nat) (h :: Nat) = BufferArray{ getArray :: IOUArray (Int, Int, Int) Word8 }

newBufferArray :: forall w h. (KnownNat w, KnownNat h) => IO (BufferArray w h)
newBufferArray = BufferArray <$> newArray ((0, 0, 0), (width - 1, height - 1, 2)) 0
  where
    width = snatToNum (SNat @w)
    height = snatToNum (SNat @h)

rasterizeBuffer
    :: forall w h. (KnownNat w, KnownNat h)
    => BufferArray w h
    -> Rasterizer w h
rasterizeBuffer (BufferArray arr) = Rasterizer $ \ptr stride -> do
    forM_ [0..height-1] $ \y -> do
        let base = y * stride
        forM_ [0..width-1] $ \x -> do
            let offset = base + (x * 4)
            pokeElemOff ptr (offset + 0) maxBound
            pokeElemOff ptr (offset + 1) =<< readArray arr (x, y, 2)
            pokeElemOff ptr (offset + 2) =<< readArray arr (x, y, 1)
            pokeElemOff ptr (offset + 3) =<< readArray arr (x, y, 0)
  where
    width = snatToNum (SNat @w)
    height = snatToNum (SNat @h)

rasterizeRay
    :: IORef (Int, Int)
    -> Rasterizer w h
    -> Rasterizer w h
rasterizeRay ray other = Rasterizer $ \ptr stride -> do
    runRasterizer other ptr stride
    (x, y) <- readIORef ray

    let offset = fromIntegral y * fromIntegral stride + (fromIntegral x * 4)
    pokeElemOff ptr (offset + 0) maxBound
    pokeElemOff ptr (offset + 1) 0
    pokeElemOff ptr (offset + 2) 0
    pokeElemOff ptr (offset + 3) maxBound


withMainWindow
    :: forall w h m. (KnownNat w, KnownNat h, MonadIO m)
    => Text
    -> CInt
    -> ([Event] -> (Scancode -> Bool) -> m (Maybe (Rasterizer w h)))
    -> m ()
withMainWindow title screenScale runFrame = do
    initializeAll
    window <- createWindow title defaultWindow
    windowSize window $= fmap (screenScale *) screenSize

    renderer <- createRenderer window (-1) defaultRenderer
    texture <- createTexture renderer RGBA8888 TextureAccessStreaming screenSize
    return (window, renderer, texture)

    let render rasterizer = do
            (ptr, stride) <- lockTexture texture Nothing
            let ptr' = castPtr ptr
            liftIO $ runRasterizer rasterizer ptr' (fromIntegral stride)
            unlockTexture texture
            SDL.copy renderer texture Nothing Nothing
            present renderer

    let loop = do
            before <- ticks
            events <- pollEvents
            keys <- getKeyboardState
            let windowClosed = any isQuitEvent events
            draw <- if windowClosed then return Nothing else runFrame events keys
            forM_ draw $ \draw -> do
                render draw
                after <- ticks
                let elapsed = after - before
                when (elapsed < frameTime) $ liftIO $ threadDelay (fromIntegral (frameTime - elapsed) * 1000)
                loop
    loop

    destroyWindow window
  where
    frameTime = 1000 `div` screenRefreshRate
    screenSize = V2 (snatToNum (SNat @w)) (snatToNum (SNat @h))

    isQuitEvent ev = case eventPayload ev of
        WindowClosedEvent{} -> True
        KeyboardEvent KeyboardEventData{ keyboardEventKeysym = Keysym{..}, ..} ->
            keyboardEventKeyMotion == Pressed && keysymKeycode == KeycodeEscape
        _ -> False
