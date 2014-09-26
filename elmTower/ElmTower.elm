import Keyboard
import Window

-- MODELs
type Platform = { x: Float, y: Float, w: Float, h: Float }
type Player = { x: Float, y: Float, w: Float, h: Float, vx: Float, vy: Float, dir: String }

type Game = {
  tick: Int,
  player: Player,
  platforms: [Platform],
  state: State}

data State = Waiting | Playing | Dead
--type Input comparable a = {x : Int, y : comparable}

-- initial state
defaultGame = {
  tick = 0,
  player = { x=0, y=0, w=35, h=35, vx=0, vy=0, dir="right" },
  platforms = [],
  state = Playing }

-- UPDATE --
jump {y} m = if y > 0 && m.y == 0 then { m | vy <- 5 } else m
gravity t m = if m.y > 0 then { m | vy <- m.vy - t/4 } else m
physics t m = { m | x <- m.x + t*m.vx , y <- max 0 (m.y + t*m.vy) }
walk {x} m = { m | vx <- toFloat x
                 , dir <- if | x < 0     -> "left"
                             | x > 0     -> "right"
                             | otherwise -> m.dir }

step: (Float, {x: Int, y: Int}) -> Player -> Player --(Float, b) -> b
step (dt, keys) =
  jump keys >> gravity dt >> walk keys >> physics dt
  
  
stepGame : (Float, {x: Int, y: Int}) -> Game -> Game
stepGame input game = 
  { game | tick <- (\n -> n + 1) game.tick
         , player <- step input game.player
         , platforms <- game.platforms
         , state <- game.state}

platform : Float -> Float -> Float -> Int -> Form
platform w h n tick = rect 50 10 |> filled (rgb 124 200 100) 
                             |> move(-w / 2 + (20 * n) + 200 + sin((toFloat tick * n) / 300) * 65, -h / 2 + (60 * n))
                             
tenToOne = let (_, l) = foldl (\j (i, acc) -> (i+1, (i + 1) :: acc)) (0, []) (repeat 10 1) in l

platforms w h tick = map (\n -> platform w h n tick) tenToOne


gameState = foldp stepGame defaultGame input 


-- DISPLAY

render (w',h') gameState =
  let (w,h) = (toFloat w', toFloat h')
      verb = if | gameState.player.y  >  0 -> "jump"
                | gameState.player.vx /= 0 -> "walk"
                | otherwise     -> "stand"
      src = "/imgs/mario/" ++ verb ++ "/" ++ gameState.player.dir ++ ".gif"
      mario = gameState.player
  in collage w' h'
      ([ rect w h  |> filled (rgb 174 238 238),
        rect w 50 |> filled (rgb 74 63 141)
                  |> move (0, 24 - h/2),
--        ngon ((round mario.x) % 10) 25.0  |> filled (rgb 24 0 0) 
--                                          |> move(-w / 2 + 300, -h / 2 + 160),
        toForm (image 35 35 src) |> move (mario.x, mario.y + 62 - h/2)
      ] ++ (platforms w h gameState.tick))

-- MARIO
input = let delta = lift (\t -> t/20) (fps 30)
        in  sampleOn delta (lift2 (,) delta Keyboard.arrows)

main = lift2 render Window.dimensions gameState
