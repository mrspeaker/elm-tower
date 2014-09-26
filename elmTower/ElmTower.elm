import Keyboard
import Window
import Color

-- MODELs
type Platform = { x: Float, y: Float, w: Float, h: Float }
type Player = { x: Float, y: Float, w: Float, h: Float, vx: Float, vy: Float, dir: String, colour: Color.Color}

type Game = {
  tick: Int,
  player: Player,
  platforms: [Platform],
  state: State}

data State = Waiting | Playing | Dead

-- initial state
defaultGame = {
  tick = 0,
  player = { x=0, y=0, w=35, h=35, vx=0, vy=0, dir="right", colour = (rgb 0 0 0) },
  platforms = createPlatforms,
  state = Playing }


createPlatforms = map (\n -> {
  x = 30 * n + 10, 
  y = toFloat ((50 * n + 10) % 280),
  w = 60,
  h = 10}) tenToOne
--createPlatforms = [
--    {x = 10, y = 0, w = 60, h = 10},
--    {x = 30, y = 50, w = 60, h = 10},
--    {x = 50, y = 100, w = 60, h = 10},
--    {x = 70, y = 150, w = 60, h = 10},
--    {x = 90, y = 200, w = 60, h = 10}]

-- update player --
jump {y} m = if y > 0 then { m | vy <- 6 } else m
gravity t m = if m.y > 0 then { m | vy <- m.vy - t/4 } else m
physics t m = { m | x <- m.x + t*m.vx , y <- max 0 (m.y + t*m.vy) }
walk {x} m = { m | vx <- toFloat x * 2
                 , dir <- if | x < 0     -> "left"
                             | x > 0     -> "right"
                             | otherwise -> m.dir }

step: [Platform] -> (Float, {x: Int, y: Int}) -> Player -> Player
step platforms (dt, keys) =
  let current = jump keys >> gravity dt >> walk keys >> physics dt
  in (\player -> 
    let
      currentPlayer = current(player)
      a = {currentPlayer |x <- currentPlayer.x - currentPlayer.w / 2
                         ,y <- currentPlayer.y - currentPlayer.h / 2}
      collidedPlatforms = filter (\plat -> 
        let b = {plat |x <- plat.x - plat.w / 2
                      ,y <- plat.y - plat.h / 2}
        in a.x + a.w >= b.x && a.x <= b.x + b.w && a.y + a.h >= b.y && a.y <= b.y + b.h ) platforms
    in if (isEmpty collidedPlatforms) then {currentPlayer | colour <- (rgb 0 0 0)} else {currentPlayer | y <- currentPlayer.y + 10, colour <- (rgb 255 255 0), vy <- 0 })
       --a)
  
  
stepGame : (Float, {x: Int, y: Int}) -> Game -> Game
stepGame input game = 
  { game | tick <- 1 + game.tick
         , player <- step game.platforms input game.player
         , platforms <- game.platforms
         , state <- game.state}

renderPlatform : Platform -> Form
renderPlatform platform = rect platform.w platform.h |> filled (rgb 124 200 100) 
                                                     |> move (platform.x, platform.y)

--platform : Float -> Float -> Float -> Int -> Form
--platform w h n tick = rect 50 10 |> filled (rgb 124 200 100) 
--                             |> move(-w / 2 + (20 * n) + 200 + sin((toFloat tick * n) / 300) * 65, -h / 2 + (60 * n))
                             
tenToOne = let (_, l) = foldl (\j (i, acc) -> (i+1, (i + 1) :: acc)) (0, []) (repeat 10 1) in l

renderPlatforms platforms = map (\plat -> renderPlatform plat) platforms


gameState = foldp stepGame defaultGame input 


-- DISPLAY

render (w',h') gameState =
  let (w,h) = (toFloat w', toFloat h')
      verb = if | gameState.player.y  >  0 -> "jump"
                | gameState.player.vx /= 0 -> "walk"
                | otherwise     -> "stand"
      src = "/imgs/mario/" ++ verb ++ "/" ++ gameState.player.dir ++ ".gif"
      player = gameState.player
  in collage w' h'
      ([ rect w h  |> filled (rgb 174 238 238),
        rect w 50 |> filled (rgb 74 63 41)
                  |> move (0, -42),
        rect player.w player.h |> filled player.colour |> move (player.x, player.y),
        toForm (image 35 35 src) |> move (player.x, player.y)
      ] ++ (renderPlatforms gameState.platforms))

input = let delta = lift (\t -> t/20) (fps 60)
        in  sampleOn delta (lift2 (,) delta Keyboard.arrows)

main = lift2 render Window.dimensions gameState
