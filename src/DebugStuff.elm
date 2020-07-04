module DebugStuff exposing (..)



import Html exposing (Html, button, text, div, h3, h5, h4, b, br, p)
import CountDict exposing (getPercentiles)
import Dict
import Html.Attributes exposing (style, value, class, id)
import Debug


fileContent : { a | trials : Int, ys : Dict.Dict Int Int, csv : Maybe String} -> Html msg
fileContent model = 
    let
        n = 1000
        trials = model.trials
        percentiles = model.ys |> getPercentiles n trials
        
    in
    
  case model.csv of
    Nothing ->
      div []    [ 
             ]
    Just content ->
        div []
            [ p [ style "white-space" "pre" ] [ text content ]
            , model.ys
              |> Dict.toList
              |> Debug.toString
              |> text
            , model
              |> Debug.toString
              |> text
            ]
