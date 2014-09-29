import Keyboard
import Window
import Color
import Debug

{--

Note on why it's not working! 

The updated player from collidedPlayerSignal never gets passed back to playerSignal...

We have have four signals in total:

playerSignal = foldp stepPlayer [defaultGame.player] input
platformSignal = foldp stepPlatform defaultGame.platforms input
collidedPlayerSignal = lift2 checkCollisions platformSignal playerSignal
gameStateSignal = foldp stepGameState [defaultGame] collidedPlayerSignal

The playerSignal does the gravity/physics, then is sent with platfromSignal to be checked for collisions. 
The collidedPlayerSignal gets the player object from the playerSignal. If there is a collision, 
it sets the isFalling to true and replaces it in the signal.

This signal is then sent to the gameState signal for rendering: and so it renders 
correctly (isFalling is correct).

Buuut! The next frame, it starts again - and the playerSignal takes the player - but it's not 
the player from the collidedPlayerSignal, it's the player from the playerSignal: so the 
isFalling (and color) changing that is done in collidedPlayerSignal is never passed back.

--}

-- MODELs
type Platform = { x: Float, y: Float, w: Float, h: Float }

type Player = { 
  x: Float, 
  y: Float, 
  w: Float, 
  h: Float, 
  vx: Float, 
  vy: Float, 
  dir: String, 
  colour: Color.Color, 
  isFalling: Bool}

type Game = {
  tick: Int,
  --player: Player,
  platforms: [Platform],
  players: [Player],
  rev: Bool,
  state: State}

type Input = (Float, {x: Int, y: Int})

data State = Waiting | Playing | Dead

range from to = indexedMap (\i el -> from + i) (repeat (to - from) 0) 

-- initial state
defaultGame: Game
defaultGame = {
  tick = 0,
  players = [{ 
    x = 0, 
    y = 0, 
    w = 24, 
    h = 40, 
    vx = 0, 
    vy = 0, 
    dir = "right", 
    colour = (rgb 0 0 0), 
    isFalling = False }],
  rev = False,
  platforms = createPlatforms,
  state = Playing }

createPlatforms : [Platform]
createPlatforms = map (\n -> {
  x = toFloat (30 * n + 10) - 150, 
  y = toFloat ((80 * n + 10)) - 50,
  w = 60,
  h = 5}) (range 1 7) ++ [{x = 0, y = -44, w = 2000, h = 50}]

isCollided : Player -> Platform -> Bool
isCollided entityA entityB =
  let 
      a = {entityA | x <- entityA.x - entityA.w / 2
                   , y <- entityA.y - entityA.h / 2}
      b = {entityB | x <- entityB.x - entityB.w / 2
                   , y <- entityB.y - entityB.h / 2}
      isLeftGreater = a.x + a.w >= b.x
      isRightLess = a.x <= b.x + b.w
      isBottomGreater = a.y + a.h >= b.y
      isTopLess = a.y <= b.y + b.h
  in isLeftGreater && isRightLess && isBottomGreater && isTopLess

jump {y} m = if y > 0 && (not m.isFalling || m.y == 0) then { m | vy <- 6} else m
gravity t m =if m.y > 0 && m.isFalling then { m | vy <- m.vy - t/4 } else m
physics t m = { m | x <- m.x + t*m.vx , y <- max 0 (m.y + t*m.vy) }
walk {x} m = { m | vx <- toFloat x * 2
                 , dir <- if | x < 0     -> "left"
                             | x > 0     -> "right"
                             | otherwise -> m.dir }

-- Player
playerStateChange: Input -> Player -> Player
playerStateChange (dt, keys) = jump keys >> gravity dt >> walk keys >> physics dt

stepPlayer : Input -> [Player] -> [Player]
stepPlayer input playerStates =
  let player = head playerStates
      -- falling is incorrect (anythign set in checkCollisions not refelcteded here)
      falling_dbg = Debug.watch("falling-step") player.isFalling
      vx_dbg = Debug.watch ("vy") player.y
      len_dbg = Debug.watch ("play len 1:") (length playerStates)
      (_ , keys) = input
  -- If playing, save state else if holding "down" key, then "rewind"
  in (if keys.y >= 0 
      then playerStateChange input player :: playerStates
      else tail (tail playerStates))

-- playerSignal = foldp stepPlayer defaultGame.players input
--

-- Platform Signal
stepPlatform_sig: Input -> [Platform] -> [Platform]
stepPlatform_sig input platforms = platforms
-- platformSignal = foldp stepPlatform_sig defaultGame.platforms input

stepPlatform input platforms tick = indexedMap (\i plat -> 
  { plat | x <- plat.x + sin((toFloat tick + toFloat (i + 1) * 10) / 20) * 5}) platforms
--

--
checkCollisions : [Platform] -> [Player] -> [Player]
checkCollisions platforms (player :: restPlayers) = 
  let collidedPlatforms = filter (isCollided player) platforms
      newPlayer = if (isEmpty collidedPlatforms) 
                  then { player | colour <- (rgb 0 255 0)
                                , isFalling <- True } 
                  else { player | colour <- (rgb 255 0 0)
                                , vy <- 0
                                , isFalling <- False}
  in newPlayer :: restPlayers

-- collidedPlayerSignal = checkCollisions <~ platformSignal ~ playerSignal

-- Game step
  {-
stepGameState: [Player] -> [Game] -> [Game]
stepGameState players (gameState :: restGameSates) = 
  -- isFalling is correctly set here. But not passed back to the next frame.
  let fall_dgb = Debug.watch("falling-game") (head players).isFalling
      len_dgb = Debug.watch("player len:") (length players)
  in { gameState | tick <- gameState.tick + 1
      -- , player <- player
      , players <- players
      , platforms <- gameState.platforms
      , state <- gameState.state} :: restGameSates
      
gameStateSignal = foldp stepGameState [defaultGame] collidedPlayerSignal
  -}

  -- This works - 'cause it doesn't use signals!
stepGameState: Input -> [Game] -> [Game]
stepGameState input (gameState :: restGameSates) = 
  let (_, keys) = input
      play = stepPlayer input gameState.players
      plat = stepPlatform input gameState.platforms gameState.tick
      collisionResolution = checkCollisions plat play
  in { gameState | tick <- gameState.tick + 1
      , players <- collisionResolution
      , platforms <- plat
      , rev <- keys.y < 0
      , state <- gameState.state} :: restGameSates

gameStateSignal = foldp stepGameState [defaultGame] input
--


-- DISPLAY
renderPlatform : Platform -> Form
renderPlatform platform = rect platform.w platform.h |> filled (rgb 252 205 1)
                                                     |> move (platform.x, platform.y)
renderPlatformStroke platform = rect platform.w 12 |> filled (rgb 152 105 1)
                                                     |> move (platform.x, platform.y - 5)
renderPlatforms platforms = map renderPlatform platforms
renderPlatformsStroke platforms = map renderPlatformStroke platforms
renderGround w h = [rect w (h / 2) |> filled (rgb 136 95 0) |> move (0, -(h / 4) - 20)]

grad : Gradient
grad =
    linear (0,60) (0,-60)
        [ (0, rgb 149 195 255)
        , (0.79, white)
        , (0.8, rgb 38 192 0)
        , (1, white)
        ]

bush = image 420 69 "https://raw.githubusercontent.com/mrspeaker/elm-tower/master/elmTower/imgs/bush.png"

render (w',h') (gameState :: _) =
  let (w,h) = (toFloat w', toFloat h')
      player = head gameState.players
      verb = if | player.vy /= 0 -> "jump"
                | player.vx /= 0 -> "walk"
                | otherwise     -> "stand"
      src = "/imgs/mario/" ++ verb ++ "/" ++ player.dir ++ ".gif"
  in collage w' h'
      ([ rect w h  |> (if gameState.rev then filled (rgb 149  195 83) else gradient grad),
        --rect player.w player.h |> filled player.colour |> move (player.x, player.y),
        toForm (bush) |> move (-50, 16),
        toForm (image 48 48 src) |> move (player.x, player.y)
      ] ++ (renderPlatforms gameState.platforms) ++ (renderPlatformsStroke gameState.platforms) ++ (renderGround w h))

input = let delta = lift (\t -> t/20) (fps 30)
        in  sampleOn delta (lift2 (,) delta Keyboard.arrows)

main = lift2 render Window.dimensions gameStateSignal
