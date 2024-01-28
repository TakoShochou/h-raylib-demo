module Arkanoid (run) where

import Import
import qualified ImportRaylib as R
import RIO.List (unfoldr)
import Control.Monad.Loops(unfoldrM)

--
-- constants
--

logoScreenTime :: Integer
logoScreenTime = 60

bricksLines :: Float -- y axis
bricksLines  = 5

bricksPerLine :: Float -- x axis
bricksPerLine = 20

--
-- types
--

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
    { player_maxLife :: Int
    , player_currentLife :: Int
    , player_pos :: R.Vector2
    , player_speed :: R.Vector2
    , player_size :: R.Vector2
    , player_bounds :: R.Rectangle
    } deriving Show

data Ball = Ball
    { ball_pos :: R.Vector2
    , ball_speed :: R.Vector2
    , ball_radius :: Float
    , ball_active :: Bool
    } deriving Show

data Brick = Brick
    { brick_pos :: R.Vector2
    , brick_size :: R.Vector2
    , brick_bounds :: R.Rectangle
    , brick_boldColor :: Bool
    , brick_active :: Bool
    } deriving Show

data GameState = GameState
    { state_screen :: GameScreen
    , state_width :: Int
    , state_height :: Int
    , state_frameCounter :: Integer
    , state_result :: Maybe GameResult
    , state_paused :: Bool
    , state_player :: Player
    , state_ball :: Ball
    , state_bricks :: [[Brick]]
    } deriving Show

--
-- premitive lenses
--

_playerCurrenLifeL :: Lens' Player Int
_playerCurrenLifeL = lens player_currentLife $ \x player_currentLife -> x {player_currentLife}

_playerPosL :: Lens' Player R.Vector2
_playerPosL = lens player_pos $ \x player_pos -> x {player_pos}

_playerSpeedL :: Lens' Player R.Vector2
_playerSpeedL = lens player_speed $ \x player_speed -> x {player_speed}

_playerSizeL :: Lens' Player R.Vector2
_playerSizeL = lens player_size $ \x player_size -> x {player_size}

_playerBoundsL :: Lens' Player R.Rectangle
_playerBoundsL = lens player_bounds $ \x player_bounds -> x {player_bounds}

_ballPosL :: Lens' Ball R.Vector2
_ballPosL = lens ball_pos $ \x ball_pos -> x {ball_pos}

_ballRadiusL :: Lens' Ball Float
_ballRadiusL = lens ball_radius $ \x ball_radius -> x {ball_radius}

_brickPosL :: Lens' Brick R.Vector2
_brickPosL = lens brick_pos $ \x brick_pos -> x {brick_pos}

_brickSizeL :: Lens' Brick R.Vector2
_brickSizeL = lens brick_size $ \x brick_size -> x {brick_size}

--
-- top level lenses (including composed ones)
--

screenL :: Lens' GameState GameScreen
screenL = lens state_screen $ \x state_screen -> x {state_screen}

widthL :: Lens' GameState Int
widthL = lens state_width $ \x state_width -> x {state_width}

heightL :: Lens' GameState Int
heightL = lens state_height $ \x state_height -> x {state_height}

frameCounterL :: Lens' GameState Integer
frameCounterL = lens state_frameCounter $ \x state_frameCounter -> x {state_frameCounter}

resultL :: Lens' GameState (Maybe GameResult)
resultL = lens state_result $ \x state_result -> x {state_result}

pausedL :: Lens' GameState Bool
pausedL = lens state_paused $ \x state_paused -> x {state_paused}

playerL :: Lens' GameState Player
playerL = lens state_player $ \x state_player -> x {state_player}

playerCurrentLifeL :: Lens' GameState Int
playerCurrentLifeL = playerL . _playerCurrenLifeL

playerPosXL :: Lens' GameState Float
playerPosXL = playerL . _playerPosL . R._vector2'x

playerPosYL :: Lens' GameState Float
playerPosYL = playerL . _playerPosL . R._vector2'y

playerSpeedXL :: Lens' GameState Float
playerSpeedXL = playerL . _playerSpeedL . R._vector2'x

playerSizeXL :: Lens' GameState Float
playerSizeXL = playerL . _playerSizeL . R._vector2'x

playerSizeYL :: Lens' GameState Float
playerSizeYL = playerL . _playerSizeL . R._vector2'y

playerBoundsL :: Lens' GameState R.Rectangle
playerBoundsL = playerL . _playerBoundsL

ballL :: Lens' GameState Ball
ballL = lens state_ball $ \x state_ball -> x {state_ball}

ballPosL :: Lens' GameState R.Vector2
ballPosL = ballL . _ballPosL

ballRadiusL :: Lens' GameState Float
ballRadiusL = ballL . _ballRadiusL

bricksL :: Lens' GameState [[Brick]]
bricksL = lens state_bricks $ \x state_bricks -> x {state_bricks}

brickPosXL :: Lens' Brick Float
brickPosXL = _brickPosL . R._vector2'x

brickPosYL :: Lens' Brick Float
brickPosYL = _brickPosL . R._vector2'y

brickSizeXL :: Lens' Brick Float
brickSizeXL = _brickSizeL . R._vector2'x

brickSizeYL :: Lens' Brick Float
brickSizeYL = _brickSizeL . R._vector2'y

--
-- init and loop
--

run :: () -> RIO App ()
run _ = do
    env <- ask
    let
        width :: Int = env.appOptions.optionsWidth
        height :: Int = env.appOptions.optionsHeight
        state_player = Player
            { player_maxLife = 3
            , player_currentLife = 3
            , player_pos = R.Vector2 (fromIntegral width/2 - 50) (fromIntegral height*7/8)
            , player_speed = R.Vector2 8 8
            , player_size = R.Vector2 (fromIntegral width / 10) (fromIntegral height / 30)
            , player_bounds = R.Rectangle 0 0 1 1
            }
        state_ball = Ball
            { ball_pos = R.Vector2
                (state_player.player_pos.vector2'x + state_player.player_size.vector2'x/2)
                (state_player.player_pos.vector2'y - state_ball.ball_radius*4)
            , ball_speed = R.Vector2 4 4
            , ball_radius = fromIntegral width / 50
            , ball_active = False
            }
        initialState = GameState
            { state_screen = Logo
            , state_width = width
            , state_height = height
            , state_frameCounter = 0
            , state_result = Nothing
            , state_paused = False
            , state_player
            , state_ball
            , state_bricks = makeBricks (fromIntegral width) bricksPerLine bricksLines
            }
    liftIO $ do
        R.setExitKey R.KeyNull
        R.withWindow width height "Arkanoid" 60 (loop initialState)
    logInfo "END GAME"

makeBricks :: Float -> Float -> Float -> [[Brick]]
makeBricks width limX limY = unfoldr fy (round limY)
    where
        fy :: Int -> Maybe ([Brick], Int)
        fy y = if y < 1 then Nothing else Just (unfoldr (fx y) (round limX), y - 1)
        fx :: Int -> Int -> Maybe (Brick, Int)
        fx y x = if x < 1 then Nothing else Just (makeBrick (fromIntegral x) (fromIntegral y), x - 1)
        brick_size = R.Vector2 (width / bricksPerLine ) 20
        makeBrick :: Float -> Float -> Brick
        makeBrick x y = Brick
            { brick_pos = R.Vector2 posX posY
            , brick_size
            , brick_bounds = R.Rectangle posX posY brick_size.vector2'x brick_size.vector2'y
            , brick_boldColor = even @Int . round $ x + y
            , brick_active = True
            }
            where
                posX = brick_size.vector2'x * x - brick_size.vector2'x
                posY = brick_size.vector2'y * y - brick_size.vector2'y

loop :: GameState -> R.WindowResources -> IO ()
loop st window = do
    newState <- update st
    render newState
    shouldClose <- R.windowShouldClose
    unless shouldClose $ loop newState window

---
--- update
---

update :: GameState -> IO GameState
update old = do
    case view screenL old of
        Logo ->
            if view frameCounterL old > logoScreenTime
                then pure $ set screenL Title $ set frameCounterL 0 old
                else pure $ over frameCounterL (+ 1) old
        Title -> do
            let st = over frameCounterL (+ 1) old
            playGame <- R.isKeyPressed R.KeySpace
            if playGame
                then pure $ set screenL GamePlay st
                else pure st
        GamePlay -> do
            pausePressed <- R.isKeyPressed R.KeySpace
            if pausePressed
                then pure $ set pausedL (not (view pausedL old)) old
                else handlePlayerPosition old >>= handleBallPosition
        Ending -> do
            goToTitle <- R.isKeyPressed R.KeySpace
            if goToTitle
                then pure $ set screenL Title old
                else pure $ set frameCounterL 0 old

handlePlayerPosition :: GameState -> IO GameState
handlePlayerPosition old = do
    leftPressed <- R.isKeyDown R.KeyLeft
    rightPressed <- R.isKeyDown R.KeyRight
    let nextPos :: Float
            = view playerPosXL old
            - (if leftPressed then view playerSpeedXL old else 0)
            + (if rightPressed then view playerSpeedXL old else 0)
    let widthFloat = fromIntegral $ view widthL old
    let nextPos'
            | nextPos < 0 = 0
            | widthFloat - view playerSizeXL old < nextPos = widthFloat - view playerSizeXL old
            | otherwise = nextPos
    let s1 = set playerPosXL nextPos' old
    let bounds = R.Rectangle
            (view playerPosXL s1)
            (view playerPosYL s1)
            (view playerSizeXL s1)
            (view playerSizeYL s1)
    pure $ set playerBoundsL bounds s1

handleBallPosition :: GameState -> IO GameState
handleBallPosition old = do
    -- TODO ball active
    -- TODO ball inactive
    pure old

---
--- render
---

render :: GameState -> IO ()
render st = do
    R.drawing $ do
        R.clearBackground R.rayWhite
        case view screenL st of
            Logo -> R.drawText "logo" 20 20 40 R.black
            Title -> do
                R.drawText "title" 20 20 40 R.black
                when (even (view frameCounterL st `div` 30)) $
                    R.drawText "press space to play game" 20 60 40 R.black
            GamePlay -> do
                R.drawRectangle
                    (round $ view playerPosXL st)
                    (round $ view playerPosYL st)
                    (round $ view playerSizeXL st)
                    (round $ view playerSizeYL st)
                    R.black
                R.drawCircleV (view ballPosL st) (view ballRadiusL st) R.maroon
                forM_ (view bricksL st) $ \xs ->
                    forM_ xs $ \x ->
                        when x.brick_active $
                        R.drawRectangle
                            (round (view brickPosXL x))
                            (round (view brickPosYL x))
                            (round (view brickSizeXL x))
                            (round (view brickSizeYL x))
                            (if x.brick_boldColor then R.gray else R.lightGray)
                void $ flip unfoldrM (view playerCurrentLifeL st) $ \i -> do
                    liftIO $ R.drawRectangle
                        (20 + 40 * i)
                        (view heightL st - 30)
                        35
                        10
                        R.lightGray
                    pure $ if i <= 1
                        then Nothing
                        else Just (i, i-1)
                when (view pausedL st) $ R.drawText "pause" 20 60 40 R.black
            Ending -> do
                R.drawText "ending" 20 20 40 R.black
                R.drawText "press space to play again" 20 60 40 R.black
