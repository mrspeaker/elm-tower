# Elm Tower

## Setup

First make sure you have [Elm Platform][platform] 0.13 or higher. The next
step is to clone this repo:

[platform]: https://github.com/elm-lang/elm-platform/

```shell
git clone https://github.com/michaelbjames/elm-examples.git
cd elm-examples
```

```shell
cd elm-tower
elm-get install
elm --make --only-js ElmTower.elm
cd ..
```

Finally, you can start the reactor from the root of this project and start
playing with the examples:

```shell
elm-reactor
```

The Reactor should be running at [http://localhost:8000][localhost].
