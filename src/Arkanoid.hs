module Arkanoid (run) where

import Import
import qualified ImportRaylib as R
import RIO.List (unfoldr)
import Control.Monad.Loops(unfoldrM)

--
-- constants
--

logoScreenTime :: Integer
logoScreenTime = 30

bricksLines :: Float -- y axis
bricksLines  = 5

bricksPerLine :: Float -- x axis
bricksPerLine = 20

playerLife :: Int
playerLife = 5

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

_ballSpeedL :: Lens' Ball R.Vector2
_ballSpeedL = lens ball_speed $ \x ball_speed -> x {ball_speed}

_ballRadiusL :: Lens' Ball Float
_ballRadiusL = lens ball_radius $ \x ball_radius -> x {ball_radius}

_ballActiveL :: Lens' Ball Bool
_ballActiveL = lens ball_active $ \x ball_active -> x {ball_active}

_brickPosL :: Lens' Brick R.Vector2
_brickPosL = lens brick_pos $ \x brick_pos -> x {brick_pos}

_brickSizeL :: Lens' Brick R.Vector2
_brickSizeL = lens brick_size $ \x brick_size -> x {brick_size}

_brickBoundsL :: Lens' Brick R.Rectangle
_brickBoundsL = lens brick_bounds $ \x brick_bounds -> x {brick_bounds}

_brickActiveL :: Lens' Brick Bool
_brickActiveL = lens brick_active $ \x brick_active -> x {brick_active}

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

ballPosXL :: Lens' GameState Float
ballPosXL = ballPosL . R._vector2'x

ballPosYL :: Lens' GameState Float
ballPosYL = ballPosL . R._vector2'y

ballSpeedL :: Lens' GameState R.Vector2
ballSpeedL = ballL . _ballSpeedL

ballSpeedXL :: Lens' GameState Float
ballSpeedXL = ballL . _ballSpeedL . R._vector2'x

ballSpeedYL :: Lens' GameState Float
ballSpeedYL = ballL. _ballSpeedL . R._vector2'y

ballRadiusL :: Lens' GameState Float
ballRadiusL = ballL . _ballRadiusL

ballActiveL :: Lens' GameState Bool
ballActiveL = ballL . _ballActiveL

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
        playerWidth :: Float = fromIntegral width / 10
        playerHeight :: Float = fromIntegral height / 30
        playerPosX :: Float = fromIntegral width / 2 - playerWidth / 2
        playerPosY :: Float = fromIntegral height*7/8
        state_player = Player
            { player_maxLife = playerLife
            , player_currentLife = playerLife
            , player_pos = R.Vector2 playerPosX playerPosY
            , player_speed = R.Vector2 8 8
            , player_size = R.Vector2 playerWidth playerHeight
            , player_bounds = R.Rectangle 0 0 1 1
            }
        state_ball = Ball
            { ball_pos = R.Vector2
                (playerPosX + playerWidth/2)
                (playerPosY - state_ball.ball_radius*4)
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
        -- TODO not working R.setExitKey R.KeyNull
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
            let s1 = over frameCounterL (+ 1) old
            playGame <- R.isKeyPressed R.KeySpace
            if playGame
                then pure $ set screenL GamePlay s1
                else pure s1
        GamePlay -> do
            pausePressed <- R.isKeyPressed R.KeyP
            s1 <- if pausePressed
                then pure $ set pausedL (not (view pausedL old)) old
                else pure old
            if view pausedL s1
                then pure s1
                else handlePlayerPosition s1 >>= handleBallPosition
            -- TODO game clear logic
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
     if view ballActiveL old
        then whenActive old
        else whenInactive old
    where
        whenActive :: GameState -> IO GameState
        whenActive st = do
            handleBallMovement st
                >>= handleBallVsScreenCollision
                >>= handleBallVsPlayerCollision
                >>= handleBallVsBricksCollision
                >>= handleGameEndingLogic
                >>= handleRetryLogic
        whenInactive :: GameState -> IO GameState
        whenInactive st = do
            let ballPosX :: Float = view playerPosXL st + view playerSizeXL st / 2
            spacePressed <- R.isKeyPressed R.KeySpace
            pure $ if spacePressed
                then set ballActiveL True
                    $ set ballSpeedL (R.Vector2 0 (-5))
                    $ set ballPosXL ballPosX st
                else set ballPosXL ballPosX st

handleBallMovement :: GameState -> IO GameState
handleBallMovement s0 = do
    pure $ over ballPosXL (+ view ballSpeedXL s0)
        $ over ballPosYL (+ view ballSpeedYL s0) s0

handleBallVsScreenCollision :: GameState -> IO GameState
handleBallVsScreenCollision s0 = do
    let s1 = if p1 >= p2 || p3 <= 0
        then over ballSpeedXL (* (-1)) s0
        else s0
    pure $ if view ballPosYL s1 <= 0
        then over ballSpeedYL (* (-1)) s1
        else s1
    where
        p1 = view ballPosXL s0 + view ballRadiusL s0
        p2 = fromIntegral (view widthL s0)
        p3 = view ballPosXL s0 - view ballRadiusL s0

handleBallVsPlayerCollision :: GameState -> IO GameState
handleBallVsPlayerCollision s0 = do
    pure $ if R.checkCollisionCircleRec
            (view ballPosL s0)
            (view ballRadiusL s0)
            (view playerBoundsL s0)
        then over ballSpeedYL (* (-1))
            $ set ballSpeedXL x
            s0
        else s0
    where
        x = (a - b) / c * 5
        a = view ballPosXL s0
        b = view playerPosXL s0 + view playerSizeXL s0 / 2
        c = view playerSizeXL s0

handleBallVsBricksCollision :: GameState -> IO GameState
handleBallVsBricksCollision s0 = do
    pure handleCollision
    where
        ballPos = s0 ^. ballPosL
        ballRadius = s0 ^. ballRadiusL
        p1 brick = brick ^. _brickActiveL
        p2 brick = R.checkCollisionCircleRec ballPos ballRadius (brick ^. _brickBoundsL)
        handleCollision :: GameState
        handleCollision = do
            let result :: [[(Brick, Bool)]] =
                    fmap (\x -> if p1 x && p2 x then (set _brickActiveL False x, True) else (x, False))
                        <$> (s0 ^. bricksL)
            let bricks = fmap fst <$> result
            let shouldBallBound = any (any snd) result
            set bricksL bricks
                $ over ballSpeedYL (if shouldBallBound then (* (- 1)) else id) s0

handleGameEndingLogic :: GameState -> IO GameState
handleGameEndingLogic s0 = do
    pure $ if round p1 >= view heightL s0
        then set ballPosXL (view playerPosXL s0 + view playerSizeXL s0 / 2)
            $ set ballPosYL (view playerPosYL s0 - view ballRadiusL s0 - 1)
            $ set ballSpeedL (R.Vector2 0 0)
            $ set ballActiveL False
            $ over playerCurrentLifeL (subtract 1) s0
        else s0
    where
        p1 = view ballPosYL s0 + view ballRadiusL s0

handleRetryLogic :: GameState -> IO GameState
handleRetryLogic s0 = do
    pure $ if s0 ^. playerCurrentLifeL < 1
        then set screenL Ending
            $ set playerCurrentLifeL playerLife
            $ set frameCounterL 0 s0
        else s0

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
                R.drawText (show $ view playerCurrentLifeL st) 20 (view heightL st - 50) 50 R.black
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
