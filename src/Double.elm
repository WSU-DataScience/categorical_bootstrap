module Double exposing (..)

type alias Double a = (a, a)

double : a -> Double a
double val = (val, val)


mapBoth : (a -> b) -> (a -> b) -> Double a -> Double b
mapBoth f g doub =
  Tuple.mapBoth f g doub


mapAll : (a -> b) -> Double a -> Double b
mapAll func doub =
  Tuple.mapBoth func func doub

add : Double number -> number
add doub =
    let
        (f, s) = doub
    in
        f + s
    