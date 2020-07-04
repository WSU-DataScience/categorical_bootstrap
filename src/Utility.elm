module Utility exposing (..)

apply : (a -> b) -> a -> b
apply f x =
    f x