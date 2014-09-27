import Keyboard
import Window
import Color
import Debug

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
  player: Player,
  platforms: [Platform],
  state: State}

type Input = (Float, {x: Int, y: Int})

data State = Waiting | Playing | Dead

range from to = indexedMap (\i el -> from + i) (repeat (to - from) 0) 

-- initial state
defaultGame: Game
defaultGame = {
  tick = 0,
  player = { 
    x = 0, 
    y = 0, 
    w = 16, 
    h = 28, 
    vx = 0, 
    vy = 0, 
    dir = "right", 
    colour = (rgb 0 0 0), 
    isFalling = False },
  platforms = createPlatforms,
  state = Playing }

createPlatforms : [Platform]
createPlatforms = map (\n -> {
  x = toFloat (30 * n + 10), 
  y = toFloat ((50 * n + 10) % 280),
  w = 60,
  h = 10}) (range 1 11) ++ [{x = 0, y = -38, w = 10000, h = 50}]

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
gravity t m = 
  let falling = Debug.watch("falling") m.isFalling
      vx = Debug.watch ("vy") m.y
  in if m.y > 0 && m.isFalling then { m | vy <- m.vy - t/4 } else m
physics t m = { m | x <- m.x + t*m.vx , y <- max 0 (m.y + t*m.vy) }
walk {x} m = { m | vx <- toFloat x * 2
                 , dir <- if | x < 0     -> "left"
                             | x > 0     -> "right"
                             | otherwise -> m.dir }

-- Player
playerStateChange: Input -> Player -> Player
playerStateChange (dt, keys) = jump keys >> gravity dt >> walk keys >> physics dt

stepPlayer : Input -> [Player] -> [Player]
stepPlayer input players =
  let latestPlayerState = head players
      (_ , keys) = input
  -- If playing, save state else if holding "down" key, then "rewind"
  in (if keys.y >= 0 
      then (playerStateChange input latestPlayerState) :: players
      else tail (tail players))

playerSignal = foldp stepPlayer [defaultGame.player] input
--

-- Platform Signal
stepPlatform: Input -> [Platform] -> [Platform]
stepPlatform input platforms = platforms

platformSignal = foldp stepPlatform defaultGame.platforms input
--

--
checkCollisions :  [Platform] -> [Player] -> [Player]
checkCollisions platforms players = 
  let player = head players
      restPlayers = tail players
      collidedPlatforms = filter (isCollided player) platforms
      fall2 = Debug.watch("Fall12") (length players)
      newPlayer = if (isEmpty collidedPlatforms) 
                  then { player | colour <- (rgb 0 0 0)
                                , isFalling <- True } 
                  else { player | colour <- (rgb 255 255 0)
                                , vy <- 0
                                , isFalling <- False}
  in (newPlayer :: restPlayers)

collidedPlayerSignal = lift2 checkCollisions platformSignal playerSignal

-- Collision detenction including player signal 

stepGameState: [Player] -> [Game] -> [Game]
stepGameState (player :: _) (gameState :: restGameSates) = 
  let playerer = Debug.watch("ph") player.isFalling
  in {gameState | tick <- gameState.tick + 1
             , player <- player
             , platforms <- gameState.platforms
             , state <- gameState.state} :: restGameSates

gameStateSignal = foldp stepGameState [defaultGame] collidedPlayerSignal

-- 


-- DISPLAY
renderPlatform : Platform -> Form
renderPlatform platform = rect platform.w platform.h |> filled (rgb 124 200 100) 
                                                     |> move (platform.x, platform.y)
renderPlatforms platforms = map renderPlatform platforms

renderGround w h = [rect w 50 |> filled (rgb 74 63 41) |> move (0, -38)]

render (w',h') (gameState :: _) =
  let (w,h) = (toFloat w', toFloat h')
      verb = if | gameState.player.isFalling -> "jump"
                | gameState.player.vx /= 0 -> "walk"
                | otherwise     -> "stand"
      src = "/imgs/mario/" ++ verb ++ "/" ++ gameState.player.dir ++ ".gif"
      player = gameState.player
      col = if gameState.player.vy == 0 then rgb 255 0 0 else rgb 0 0 255
  in collage w' h'
      ([ rect w h  |> filled (rgb 174 238 238),
        rect player.w player.h |> filled col |> move (player.x, player.y),
        toForm (image 35 35 src) |> Debug.trace("playerpath") |> move (player.x, player.y)
      ] ++ (renderPlatforms gameState.platforms) ++ (renderGround w h))

input = let delta = lift (\t -> t/20) (fps 60)
        in  sampleOn delta (lift2 (,) delta Keyboard.arrows)

main = lift2 render Window.dimensions gameStateSignal
