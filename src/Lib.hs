{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
  ( basicWindowExample
  , fontExample
  , fpExample
  , textureExample
  ) where

import Import
import qualified ImportRaylib as RL

basicWindowExample :: () -> RIO App ()
basicWindowExample _ = do
  env <- ask
  liftIO $ RL.withWindow
    env.appOptions.optionsWidth
    env.appOptions.optionsHeight
    "window example"
    60
    f
  where
    f :: (MonadIO m, MonadMask m) => RL.WindowResources -> m ()
    f _ =
      RL.whileWindowOpen0 $
        RL.drawing $ do
          liftIO $ RL.clearBackground RL.rayWhite
          liftIO $ RL.drawText "hello, world" 30 40 18 RL.lightGray

fontExample :: String -> RIO App ()
fontExample msg = do
  env <- ask
  liftIO $ RL.withWindow
    env.appOptions.optionsWidth
    env.appOptions.optionsHeight
    "font example"
    60
    f
  where
    fontPath = joinPath [".", "assets", "Lato-Regular.ttf"]
    f :: (MonadIO m) => RL.WindowResources -> m ()
    f window = do
      font <- liftIO $ RL.loadFont fontPath window
      flip RL.whileWindowOpen_ 20 $
        \size -> do
          liftIO $ RL.drawing $ do
            liftIO $ RL.clearBackground RL.rayWhite
            liftIO $ RL.drawTextEx font msg (RL.Vector2 20.0 12.0) (fromIntegral size) 1.0 RL.black
            liftIO $ RL.drawText "bar" 20 (size + 15) 24 RL.black
          increaseSize <- liftIO $ RL.isKeyPressed RL.KeyUp
          decreaseSize <- liftIO $ RL.isKeyPressed RL.KeyDown
          return $ size + fromBool increaseSize - fromBool decreaseSize

fpExample :: () -> RIO App ()
fpExample _ = do
  env <- ask
  liftIO $ RL.withWindow
    env.appOptions.optionsWidth
    env.appOptions.optionsHeight
    "first person example"
    60
    f1
  where
    f1 :: RL.WindowResources -> IO ()
    f1 _ = do
      RL.disableCursor
      let camera =
            RL.Camera3D
              (RL.Vector3 0 1 (-1))
              (RL.Vector3 0 1 0)
              (RL.Vector3 0 1 0)
              70
              RL.CameraPerspective
      RL.whileWindowOpen_ f2 camera
    f2 loop = RL.drawing (f3 loop) >> RL.updateCamera loop RL.CameraModeFirstPerson
    f3 loop = do
      RL.clearBackground RL.black
      RL.drawFPS 10 20
      RL.mode3D
        loop
        $ do
          RL.drawGrid 30 1
          RL.drawCircle3D (RL.Vector3 2 1 1) 2 (RL.Vector3 0 1 0) 0 RL.white
          RL.drawLine3D (RL.Vector3 3 0 1) (RL.Vector3 1 2 1) RL.white
          RL.drawLine3D (RL.Vector3 3 2 1) (RL.Vector3 1 0 1 ) RL.white
          RL.drawCubeWiresV (RL.Vector3 (-2) 1 0 ) (RL.Vector3 1 2 1) RL.white

textureExample :: String ->  RIO App ()
textureExample textureName = do
  env <- ask
  liftIO $ RL.withWindow
    env.appOptions.optionsWidth
    env.appOptions.optionsHeight
    "texture example"
    60
    f1
  where
    logoPath = joinPath [".", "assets", textureName]
    f1 window = do
      texture <- RL.genImagePerlinNoise 600 450 20 20 2 >>= (`RL.loadTextureFromImage` window)
      logo <- RL.loadImage logoPath >>= (`RL.loadTextureFromImage` window)
      rt <- RL.loadRenderTexture 200 200 window
      RL.whileWindowOpen0 (RL.drawing (render texture rt logo))
    render texture rt logo = do
      RL.textureMode
        rt
        $ do
          RL.clearBackground RL.lightGray
          RL.drawText "foo bar" 10 10 20 RL.black
      RL.clearBackground RL.white
      RL.drawTexture texture 0 0 RL.orange
      RL.drawTexturePro
        (RL.renderTexture'texture rt)
        (RL.Rectangle 0 0 200 (-200))
        (RL.Rectangle 50 50 300 300)
        (RL.Vector2 0 0)
        0
        RL.white
      RL.drawTexturePro
        logo
        (RL.Rectangle 0 0 256 256)
        (RL.Rectangle 375 50 175 175)
        (RL.Vector2 0 0)
        0
        RL.white
