module DataSet exposing (..)


import List.Extra exposing (find, group, zip)
import Maybe exposing (withDefault)

type Statistic = NotSelected | Count | Proportion
type alias LabelFreq =  { label : String
                        , count : Int
                        }

type alias DataSet =    { name : String
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
tumaalBloodType =   { name = "Tumaal/Midgaan Blood Types"
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

type alias Sample = { successLbl : String
                     , failureLbl : String
                     , numSuccess : Int
                     , numFailures : Int
                     , n : Int
                     }

makeEmptySample : Sample -> Sample
makeEmptySample sample =
    { sample | numSuccess = 0, numFailures = 0}

updateSample : Int -> Sample -> Sample
updateSample x sample =
    { sample | numSuccess = x, numFailures = sample.n - x }


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

getSuccess : String -> DataSet -> Maybe Sample
getSuccess txt data =
    let
        n = data 
            |> getN
    in
     data.frequencies
        |> find (\p -> .label p == txt)
        |> Maybe.map (\labelFreq -> { successLbl = .label labelFreq
                                    , failureLbl = "Not " ++ .label labelFreq
                                    , numSuccess = .count labelFreq
                                    , numFailures = n - .count labelFreq
                                    , n = n
                                    })