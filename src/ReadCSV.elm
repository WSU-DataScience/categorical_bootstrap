module ReadCSV exposing (..)

import List.Extra exposing (uncons, getAt, zip)

type FileType = NoType | Regular | Frequency

example1 = "ABO Blood Type,Count\nA,16\nAB,1\nB,11\nO,26\n"
example2 = "ABO Blood Type\nA\nO\nA\nO\nO\nO\nB\nA\nA\nO\nA\nA\nB\nB\nO\nO\nO\nO\nO\nO\nB\nB\nA\nA\nO\nO\nA\nO\nO\nB\nO\nA\nAB\nB\nA\nB\nA\nO\nO\nB\nA\nB\nB\nO\nO\nO\nO\nA\nO\nO\nA\nO\nO\nA\n"


getHeadTail : String -> Maybe (String, List String)
getHeadTail rawfile =
    let
        lines = rawfile
                |> String.trim -- trim off the extra \n on the last line
                |> String.split "\n"
    in
        uncons lines

getColLabels : (String, List String) -> List String
getColLabels pair = 
    pair 
    |> Tuple.first
    |> String.split ","

clearBlankRows : List String -> List String
clearBlankRows = List.filter (\s -> String.length s > 0)


colIdx : String -> (List String, List Int)
colIdx rawheader =
    rawheader
    |> String.split "," 
    |> \h -> (h, h)
    |> Tuple.mapSecond List.length 
    |> Tuple.mapSecond (\n -> n - 1) 
    |> Tuple.mapSecond (List.range 0)

splitBody : List String -> List (List String)
splitBody body =
    body
    |> List.map (String.split ",")

getCol : List (List String) -> Int -> List String
getCol rows i=
    rows
    |> List.map (getAt i >> Maybe.withDefault "")

getCols : ((List String, List Int), List (List String)) -> List (String, List String)
getCols pair = 
    let
        ((header, idx), rows) = pair
        cols = 
            idx
            |> List.map (getCol rows)
    in
        zip header cols

type alias Variable = (String, List String)

getVariables : String -> Maybe (List Variable)
getVariables rawfile =
    rawfile
    |> getHeadTail
    |> Maybe.map (Tuple.mapSecond clearBlankRows)
    |> Maybe.map (Tuple.mapBoth colIdx splitBody)
    |> Maybe.map getCols


findVariable : String -> List Variable -> Maybe (List String)
findVariable lbl vars =
    vars
    |> List.Extra.find (\p -> Tuple.first p == lbl) 
    |> Maybe.map Tuple.second


getFileType : String -> FileType
getFileType typeLbl = 
    case typeLbl of
        "reg" ->
            Regular

        "freq" ->
            Frequency

        _ ->
            NoType
