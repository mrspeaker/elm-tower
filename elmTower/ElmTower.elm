import Keyboard
import Window
import Color

-- MODELs
type Platform = { x: Float, y: Float, w: Float, h: Float }
type Player = { x: Float, y: Float, w: Float, h: Float, vx: Float, vy: Float, dir: String, colour: Color.Color, isFalling: Bool}

type Game = {
  tick: Int,
  player: Player,
  platforms: [Platform],
  state: State}

data State = Waiting | Playing | Dead

-- initial state
defaultGame = {
  tick = 0,
  player = { x=0, y=0, w=16, h=28, vx=0, vy=0, dir="right", colour = (rgb 0 0 0), isFalling = False},
  platforms = createPlatforms,
  state = Playing }


createPlatforms = map (\n -> {
  x = 30 * n + 10, 
  y = toFloat ((50 * n + 10) % 280),
  w = 60,
  h = 10}) tenToOne ++ [{x = 0, y = -38, w = 10000, h = 50}]

jump {y} m = if y > 0 && (not m.isFalling || m.y == 0) then { m | vy <- 6} else m
gravity t m = if m.y > 0 && m.isFalling then { m | vy <- m.vy - t/4 } else m
physics t m = { m | x <- m.x + t*m.vx , y <- max 0 (m.y + t*m.vy) }
walk {x} m = { m | vx <- toFloat x * 2
                 , dir <- if | x < 0     -> "left"
                             | x > 0     -> "right"
                             | otherwise -> m.dir }

renderPlatform : Platform -> Form
renderPlatform platform = rect platform.w platform.h |> filled (rgb 124 200 100) 
                                                     |> move (platform.x, platform.y)
                             
tenToOne = let (_, l) = foldl (\j (i, acc) -> (i+1, (i + 1) :: acc)) (0, []) (repeat 10 1) in l

renderPlatforms platforms = map (\plat -> renderPlatform plat) platforms


-- Mario SIgnal
marioStateChange: (Float, {x: Int, y: Int}) -> Player -> Player
marioStateChange (dt, keys) = jump keys >> gravity dt >> walk keys >> physics dt

stepMario : (Float, {x: Int, y: Int}) -> [Player] -> [Player]
stepMario input marioStates =
  let lastestMarioState = head marioStates
      (_ , keys) = input
  in (if keys.y >= 0 
      then [marioStateChange input lastestMarioState] ++ marioStates
      else tail (tail marioStates))

marioSignal = foldp stepMario [defaultGame.player] input
--

-- PlatformSignal
stepPlatform: (Float, {x: Int, y: Int}) -> [Platform] -> [Platform]
stepPlatform input platforms = platforms

platformSignal = foldp stepPlatform createPlatforms input
--

--
isCollided currentPlayer plat =
  let 
      a = {currentPlayer |x <- currentPlayer.x - currentPlayer.w / 2
                         ,y <- currentPlayer.y - currentPlayer.h / 2}
      b = {plat |x <- plat.x - plat.w / 2
               ,y <- plat.y - plat.h / 2}
      isLeftCollision = a.x + a.w >= b.x
      isRightCollision = a.x <= b.x + b.w
      isBottomCollision = a.y + a.h >= b.y
      isTopCollision = a.y <= b.y + b.h
  in isLeftCollision && isRightCollision && isBottomCollision && isTopCollision

combineToCollidedPlayer : [Player] -> [Platform] -> [Player]
combineToCollidedPlayer (mario :: restMarios) platforms = 
  let collidedPlatforms = filter (isCollided mario) platforms
      newPlayer = if (isEmpty collidedPlatforms) 
                  then {mario | colour <- (rgb 0 0 0), isFalling <- True} 
                  else {mario | colour <- (rgb 255 255 0)
                              , vy <- 0
                              , isFalling <- False}
  in newPlayer :: restMarios

collidedPlayerSignal = lift2 combineToCollidedPlayer marioSignal platformSignal

-- Collision detenction including mario signal 

stepGameState: [Player] -> [Game] -> [Game]
stepGameState (mario :: restMarios) (gameState :: restGameSates) = 
  {gameState | tick <- gameState.tick + 1
             , player <- mario
             , platforms <- gameState.platforms
             , state <- gameState.state} :: restGameSates

gameStateSignal = foldp stepGameState [defaultGame] collidedPlayerSignal

-- 


-- DISPLAY

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
        toForm (image 35 35 src) |> move (player.x, player.y)
      ] ++ (renderPlatforms gameState.platforms) ++ [rect w 50 |> filled (rgb 74 63 41) |> move (0, -38)])

input = let delta = lift (\t -> t/20) (fps 60)
        in  sampleOn delta (lift2 (,) delta Keyboard.arrows)

main = lift2 render Window.dimensions gameStateSignal
