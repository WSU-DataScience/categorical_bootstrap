module Defaults exposing (..)


type alias Defaults = { n : Int 
                      , p : Float
                      , trimAt : Int
                      , collectNs : List Int
                      , levels : List Float
                      , levelsTxt : List String
                      , minTrialsForPValue : Int
                      , distMinHeight : Float
                      , numSD : Float
                      , pValDigits : Int
                      , distPlotWidth : Float
                      , distPlotHeight : Float
                      , largePlot : Int
                      }

defaults :  Defaults
defaults =  { n = 200
            , p = 0.25
            , trimAt = 100
            , collectNs = [1, 10, 100, 1000, 10000]
            , levels = [0.8, 0.9, 0.95, 0.99]
            , levelsTxt = ["80%", "90%", "95%", "99%"]
            , minTrialsForPValue = 100
            , distMinHeight = 100.0
            , numSD = 4.0
            , pValDigits = 4
            , distPlotWidth = 700
            , distPlotHeight = 525  
            , largePlot = 5000
            }
