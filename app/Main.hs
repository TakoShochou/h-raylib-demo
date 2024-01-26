{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_game1
import qualified Lib
import qualified Arkanoid

main :: IO ()
main = do
  (options, runCmd) <- simpleOptions
    $(simpleVersion Paths_game1.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
      <$> switch ( long "verbose" <> short 'v' <> help "Verbose output?")
      <*> option auto (long "width" <> short 'w' <> help "window width" <> value 640 <> metavar "NATURAL_NUMBER" <> showDefault)
      <*> option auto (long "height" <> short 'h' <> help "window height" <> value 480 <> metavar "NATURAL_NUMBER" <> showDefault)
    )
    (windowCmd >> fontCmd >> fpCmd >> textureCmd >> arkanoidCmd)
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app runCmd
  where
    windowCmd = addCommand "window" "basic window example" Lib.basicWindowExample (pure ())
    fontCmd = addCommand "font" "font example" Lib.fontExample $
      strArgument (help "text to display" <> value "foo" <> showDefault)
    fpCmd = addCommand "fp" "first persopn example" Lib.fpExample (pure ())
    textureCmd = addCommand "texture" "texture example" Lib.textureExample $
      strArgument (help "texture to display" <> value "raylib-logo.png" <> showDefault)
    arkanoidCmd = addCommand "arkanoid" "play arkanoid" Arkanoid.run (pure ())
