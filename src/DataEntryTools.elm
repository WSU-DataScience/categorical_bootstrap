module DataEntryTools exposing (..)

import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Html exposing (..)
import Html.Attributes exposing (..)

type Visibility
    = Hidden
    | Shown



type EntryState
    = Blank
    | Correct
    | NotANumber
    | OutOfBounds
    | OtherwiseIncorrect


type alias NumericData a =
    { str : String
    , val : Maybe a
    , state : EntryState
    }


initInt : NumericData Int
initInt =
    { str = ""
    , val = Nothing
    , state = Blank
    }



type alias EntryConfig msg =
    { placeholder : String
    , label : String
    , onInput : String -> msg
    }


entryConfig : String -> String -> (String -> msg) -> EntryConfig msg
entryConfig placeholder label onInput =
    { placeholder = placeholder
    , label = label
    , onInput = onInput
    }


baseOptions : String -> (String -> msg) -> List (Input.Option msg)
baseOptions placeholder msg =
            [ Input.placeholder placeholder
            , Input.onInput msg
            ]

hasTabIndex : Int -> List (Input.Option msg) -> List (Input.Option msg)
hasTabIndex n opts =
            (Input.attrs [ Html.Attributes.tabindex n ]) :: opts

withValue : String -> List (Input.Option msg) -> List (Input.Option msg)
withValue value opts =
            (Input.value value) :: opts

addEntryState : EntryState -> List (Input.Option msg) -> List (Input.Option msg)
addEntryState status opts =
    case status of
        Blank ->
            opts

        Correct ->
            Input.success :: opts

        _ ->
            Input.danger :: opts


numericEntryState : (String -> Maybe a) -> (a -> Bool) -> String -> EntryState
numericEntryState convert isOutOfBounds input =
    case ( input, convert input ) of
        ( "", _ ) ->
            Blank

        ( _, Nothing ) ->
            NotANumber

        ( _, Just p ) ->
            if isOutOfBounds p then 
               Correct 

           else 
               OutOfBounds


updateNumericStr : (String -> Maybe a) -> String -> NumericData a -> NumericData a
updateNumericStr convert input numbericData =
    { numbericData
        | str = input
        , val = convert input
    }


updateNumericState : (String -> Maybe a) -> (a -> Bool) -> NumericData a -> NumericData a
updateNumericState convert isOutOfBounds numbericData =
    { numbericData
        | state = numericEntryState convert isOutOfBounds numbericData.str
    }


updateNumeric : (String -> Maybe number) -> (number -> Bool) -> String-> NumericData number -> NumericData number
updateNumeric convert isOutOfBounds input numbericData =
    numbericData
        |> updateNumericStr convert input
        |> updateNumericState convert isOutOfBounds


entryView : String -> List (Input.Option msg) -> Html msg
entryView label opts =
    InputGroup.config
        (InputGroup.text opts)
        |> InputGroup.small
        |> InputGroup.predecessors
            [ InputGroup.span [] [ Html.text label ] ]
        |> InputGroup.view


errorView : (a -> Bool) -> String -> a -> Html msg
errorView hasError msg model =
    let
        isInError =
            hasError model
    in
      if isInError then
              Html.span [ Html.Attributes.style "color" "red" ] [ Html.text msg ]
      else
              Html.span [] [ Html.text "" ]


-- Data Entry for X--The limit of a p value

isXInOfBounds : Int -> Int -> Bool
isXInOfBounds n x = 
    (x >= 0) && (x <= n)


updateXData lbl model = 
    updateNumeric String.toInt (isXInOfBounds model.n) lbl model.xData


makeHtmlText : String -> String -> Html msg
makeHtmlText header str =
    Html.text (header ++ str)

-- view helpers

basicEntry lbl placeholder tab msg state = 
    entryView lbl   (baseOptions placeholder msg
                    |> addEntryState state
                    |> hasTabIndex tab
                    )

nEntry = basicEntry "n" "" 1
pNullEntry = basicEntry "Null p" "" 2
pTruthEntry = basicEntry "True p" "" 3


hasPTruthError model =
    (model.pTruth.state == NotANumber) || (model.pTruth.state == OutOfBounds)

hasPNullError model =
    (model.pNull.state == NotANumber) || (model.pTruth.state == OutOfBounds)

pNullError = errorView hasPNullError "Null p is a number between 0 and 1."

pTruthError = errorView hasPTruthError "True p is a number between 0 and 1."

hasNError model =
    (model.nData.state == NotANumber) || (model.nData.state == OutOfBounds)


nError = errorView hasNError "n is a whole number."


xEntry msg model =
    entryView "x" (baseOptions "" msg
                    |> withValue model.xData.str
                    |> addEntryState model.xData.state
                    |> withValue model.xData.str
                    )

hasXError model =
    (model.xData.state == NotANumber) || (model.xData.state == OutOfBounds)

xError model =
    let
        msg = "x is a number between 0 and " ++ (model.n |> String.fromInt) ++ "."
    in
        errorView hasXError msg model

