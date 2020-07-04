module CountDict exposing (..)

import Dict exposing (..)
import List.Extra exposing (zip, scanl)

type alias Count = (Int, Int)

type alias CountDict = Dict Int Int 

updateY : Int -> CountDict -> CountDict
updateY x ys =
    Dict.update x updateCount ys


updateCount : Maybe Int -> Maybe Int
updateCount maybeN =
    case maybeN of
        Just n ->
            Just (n + 1)

        Nothing ->
            Just 1


updateCountDict : (Float -> Int) -> CountDict -> List Float -> CountDict
updateCountDict binomGen cnts outcomes =
  outcomes
  |> List.map binomGen
  |> List.foldl updateY cnts 


tallestBar : CountDict -> Int
tallestBar yDict =
  yDict
  |> Dict.toList
  |> List.foldl updateTallest 0


updateTallest : Count -> Int -> Int
updateTallest pair currentMax =
  let
    ( _ , newY) = pair
  in
    Basics.max newY currentMax

divideBy : Int -> Int -> Float
divideBy denom numer =
    (toFloat numer)/(toFloat denom) 

getPercentiles : Int -> Int -> CountDict -> List (Float, Float)
getPercentiles n trials counts =
        counts
        |> Dict.toList
        |> List.map (Tuple.mapBoth (divideBy n) (divideBy trials))
        |> \el -> zip (List.map Tuple.first el) (el |> List.map Tuple.second |> scanl (+) 0 )