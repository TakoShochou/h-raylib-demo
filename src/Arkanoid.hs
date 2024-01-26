module Arkanoid (run) where

import Import
import qualified ImportText as T
import qualified ImportRaylib as R
import RIO.List (unfoldr)
import Control.Monad.Loops(unfoldrM)

data GameScreen
    = Logo
    | Title
    | GamePlay
    | Ending
    deriving Show

data GameResult
    = Loose
    | Win
    deriving Show

data Player = Player
    { maxLife :: Int
    , currentLife :: Int
    , pos :: R.Vector2
    , speed :: R.Vector2
    , size :: R.Vector2
    , bounds :: R.Rectangle
    } deriving Show

data Ball = Ball
    { pos :: R.Vector2
    , speed :: R.Vector2
    , radius :: Float
    , active :: Bool
    } deriving Show

data Brick = Brick
    { pos :: R.Vector2
    , size :: R.Vector2
    , bounds :: R.Rectangle
    , active :: Bool
    } deriving Show

data GameState = GameState
    { screen :: GameScreen
    , width :: Int
    , height :: Int
    , frameCounter :: Integer
    , result :: Maybe GameResult
    , paused :: Bool
    , player :: Player
    , ball :: Ball
    , bricks :: [Brick]
    } deriving Show

run :: () -> RIO App ()
run _ = do
    env <- ask
    let
        width = env.appOptions.optionsWidth
        height = env.appOptions.optionsHeight
        player = Player
            { maxLife = 3
            , currentLife = 3
            , pos = R.Vector2 (fromIntegral width/2 - 50) (fromIntegral height*7/8)
            , speed = R.Vector2 8 8
            , size = R.Vector2 100 24
            , bounds = R.Rectangle 0 0 1 1
            }
        ball = Ball
            { pos = R.Vector2
                (player.pos.vector2'x + player.size.vector2'x/2)
                (player.pos.vector2'y - ball.radius*2)
            , speed = R.Vector2 4 4
            , radius = 10
            , active = False
            }
        initialState = GameState
            { screen = Logo
            , width
            , height
            , frameCounter = 0
            , result = Nothing
            , paused = False
            , player
            , ball
            , bricks = makeBricks 10 2
            }
    liftIO $ do
        R.setExitKey R.KeyNull
        R.withWindow width height "Arkanoid" 60 (loop initialState)
    logInfo "END GAME"

-- TODO
makeBricks :: Int -> Int -> [Brick]
makeBricks x y =
    unfoldr f x
    where
        f b =
            if b < x
                then Nothing
                else Just (makeBrick b, b - 1)
        makeBrick :: Int -> Brick
        makeBrick b = Brick (R.Vector2 (fromIntegral b) 0) ( R.Vector2 0 0) (R.Rectangle 0 0 1 1) True

loop :: GameState -> R.WindowResources -> IO ()
loop st window = do
    T.hPutStrLn stderr $ "loop" <> tshow st
    newState <- update st
    render newState
    shouldClose <- R.windowShouldClose
    unless shouldClose $ loop newState window

update :: GameState -> IO GameState
update old = do
    case old.screen of
        Logo ->
            if old.frameCounter > 60
                then pure old {screen = Title, frameCounter = 0}
                else pure old {frameCounter = old.frameCounter + 1}
        Title -> do
            let st = old {frameCounter = old.frameCounter + 1}
            playGame <- R.isKeyPressed R.KeySpace
            if playGame
                then pure st {screen = GamePlay}
                else pure st
        GamePlay -> do
            pausePressed <- R.isKeyPressed R.KeySpace
            let st = if pausePressed
                then
                    if old.paused then old {paused = False} else old {paused = True}
                else old
            pure st
        Ending -> do
            goToTitle <- R.isKeyPressed R.KeySpace
            if goToTitle
                then pure old {screen = Title}
                else pure old {frameCounter = 0}

render :: GameState -> IO ()
render st = do
    R.drawing $ do
        R.clearBackground R.rayWhite
        case st.screen of
            Logo -> R.drawText "logo" 20 20 40 R.black
            Title -> do
                R.drawText "title" 20 20 40 R.black
                when (even (st.frameCounter `div` 30)) $
                    R.drawText "press space to play game" 20 60 40 R.black
            GamePlay -> do
                R.drawRectangle
                    (round st.player.pos.vector2'x)
                    (round st.player.pos.vector2'y)
                    (round st.player.size.vector2'x)
                    (round st.player.size.vector2'y)
                    R.black
                R.drawCircleV st.ball.pos st.ball.radius R.maroon
                void $ flip unfoldrM st.player.currentLife $ \i -> do
                    liftIO $ R.drawRectangle
                        (20 + 40 * i)
                        (st.height - 30)
                        35
                        10
                        R.lightGray
                    pure $ if i <= 1
                        then Nothing
                        else Just (i, i-1)
                when st.paused $ R.drawText "pause" 20 60 40 R.black
            Ending -> do
                R.drawText "ending" 20 20 40 R.black
                R.drawText "press space to play again" 20 60 40 R.black
