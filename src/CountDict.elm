module CountDict exposing (..)

import Dict exposing (..)
import List.Extra exposing (zip, scanl)
import Tuple.Extra exposing (apply)
import Utility exposing (roundFloat)
import Maybe exposing (withDefault)

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


leverage : Int -> Float -> Int -> Float
leverage trials val cnt  =
    let
        frac = (toFloat cnt) / (toFloat trials)
    in
        val * frac

expectedValue : Int -> List (Float, Int) -> Float
expectedValue trials valAndFreq =
    valAndFreq
    |> List.map (Tuple.mapSecond (divideBy trials))
    |> List.map (Tuple.Extra.apply (*))
    |> List.foldl (+) 0
    |> roundFloat 3


meanWithTransform : (Int -> Float) -> Int -> CountDict -> Float
meanWithTransform f trials countDict =
    countDict
    |> Dict.toList
    |> List.map (Tuple.mapFirst f)
    |> expectedValue trials

meanProp : Int -> CountDict -> { a | n : Int } -> Float
meanProp trials countDict sample =
    meanWithTransform (divideBy sample.n) trials countDict 

mean : Int -> CountDict -> Float
mean = meanWithTransform toFloat

residSqr : Float -> Float -> Float
residSqr m x = (x - m)^2


sdWithTranform : (Int -> Float) -> Int -> CountDict -> Float
sdWithTranform f trials countDict = 
    let
        m = meanWithTransform f trials countDict 
        -- adjust so that we divide by N - 1
        adjust = trials |> divideBy (trials - 1)
        trans = f >> residSqr m >> (*) adjust
    in
        countDict
        |> Dict.toList
        |> List.map (Tuple.mapFirst trans)
        |> List.map (Tuple.mapSecond (divideBy trials))
        |> List.map (Tuple.Extra.apply (*))
        |> List.foldl (+) 0
        |> sqrt

sdProp : Int -> CountDict -> { a | n : Int } -> Float
sdProp trials countDict sample = 
    sdWithTranform (divideBy sample.n) trials countDict

standDev : Int -> CountDict -> Float
standDev = sdWithTranform toFloat