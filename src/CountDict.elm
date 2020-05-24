module CountDict exposing (..)

import Dict exposing (..)

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