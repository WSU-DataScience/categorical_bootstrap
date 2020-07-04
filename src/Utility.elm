module Utility exposing (..)

apply : (a -> b) -> a -> b
apply f x =
    f x


roundFloat : Int -> Float -> Float
roundFloat digits n =
   let
     div = toFloat 10^(toFloat digits)
     shifted = n*div
   in
     round shifted |> toFloat |> \x -> x/div