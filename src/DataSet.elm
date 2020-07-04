module DataSet exposing (..)


import List.Extra exposing (find, group, zip)
import Binomial exposing (roundFloat)
import Maybe exposing (withDefault)
import Maybe.Extra exposing (isNothing, unwrap)
import Template exposing (template, render, withValue, withString)
import Html exposing (Html, button, text, div, h3, h5, h4, b, br)
import Html.Attributes exposing (style, value, class, id)

type Statistic = NotSelected | Count | Proportion
type alias LabelFreq =  { label : String
                        , count : Int
                        }

type alias DataSet =    { name : String -- Should be ~18 characters or less
                        , frequencies : List LabelFreq
                        , labels : List String
                        , counts : List Int
                        }

somaliFreq : List (String, Int)
somaliFreq =    [ ("O", 574)
                , ("A1", 130)
                , ("B", 166)
                , ("A2 or AB", 130)
                ]

makeLabelFreq : (String, Int) -> LabelFreq
makeLabelFreq pair = 
    let
        (lbl, cnt) = pair
    in
        LabelFreq lbl cnt
    
somaliBloodType : DataSet
somaliBloodType =   { name = "Somali Blood Types"
                    , frequencies = List.map makeLabelFreq somaliFreq
                    , labels = List.map Tuple.first somaliFreq
                    , counts = List.map Tuple.second somaliFreq
                    }


tumaalFreq : List (String, Int)
tumaalFreq =    [ ("A", 16)
                , ("AB", 1)
                , ("B", 11)
                , ("O", 26)
                ]


tumaalBloodType : DataSet
tumaalBloodType =   { name = "Tumaal Blood Types"
                    , frequencies = List.map makeLabelFreq tumaalFreq
                    , labels = List.map Tuple.first tumaalFreq
                    , counts = List.map Tuple.second tumaalFreq
                    }


datasets : List DataSet
datasets =  [ somaliBloodType
            , tumaalBloodType
            ]

createDataFromRegular : String -> List String -> DataSet
createDataFromRegular name catcol =
    let
        freqs = catcol
              |> List.sort
              |> group
              |> List.map (Tuple.mapSecond (List.length >> (+) 1))
    in
        { name = name
        , frequencies = List.map makeLabelFreq freqs
        , labels = List.map Tuple.first freqs
        , counts = List.map Tuple.second freqs
        }


createDataFromFreq : String -> List String -> List Int -> DataSet
createDataFromFreq name lbls cnts =
    { name = name
    , frequencies = List.map makeLabelFreq (zip lbls cnts)
    , labels = lbls
    , counts = cnts
    }


getNewData : String -> List DataSet -> Maybe DataSet
getNewData dataName = find (\d -> d.name == dataName)

getLabels : DataSet -> List String
getLabels data =
    data.frequencies
    |> List.map .label


getCounts : DataSet -> List Int
getCounts data =
    data.frequencies
    |> List.map .count

getN : DataSet -> Int
getN data =
    data
    |> getCounts
    |> List.foldl (+) 0
