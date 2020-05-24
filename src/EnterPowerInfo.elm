module EnterPowerInfo exposing (..)


import DataEntryTools exposing (..)
import Layout exposing (..)
import Maybe exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.CDN as CDN
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row


-- This pages main

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Model

type alias Model = { n : Int
                   , nData : NumericData Int
                   , pNull : NumericData Float
                   , pTruth : NumericData Float
                   , xData : NumericData Int
                   }

-- Initialize

initFloat : NumericData Float
initFloat = {str = "", val = Nothing, state = Blank}

initModel = { n = 20
            ,nData = initInt
            , pNull = initFloat
            , pTruth = initFloat
            , xData = initInt
            }


init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none )

-- Messages

type Msg  = ChangeN String
          | ChangePNull String
          | ChangePTruth String
          | ChangeX String


-- update functions

isPOfBounds p = (p >= 0) && (p <= 1)

updatePData : String -> NumericData Float -> NumericData Float 
updatePData = updateNumeric String.toFloat isPOfBounds

isNInOfBounds n = n > 0

isXInOfBounds n x = (x > 0) && (x <= n)

nEntryState = numericEntryState String.toInt isNInOfBounds

xEntryState = numericEntryState String.toInt isXInOfBounds

updateNData : String -> NumericData Int -> NumericData Int 
updateNData = updateNumeric String.toInt isNInOfBounds


updateXData : String -> NumericData Int -> NumericData Int 
updateXData = updateNumeric String.toInt isXInOfBounds

updateN : String -> Model -> Model
updateN input model =
    let 
        newNData = model.nData |> updateNData input
    in
        {model | n = withDefault model.n newNData 
        , nData = newNData}

updateX : String -> Model -> Model
updateX input model =
  {model | nData = model.nData |> updateXData input }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangePNull text ->
        ( {model | pNull = model.pNull |> updatePData text}
        , Cmd.none
        )

    ChangePTruth text ->
        ( {model | pTruth = model.pTruth |> updatePData text}
        , Cmd.none
        )

    ChangeN text ->
        ( model
            |> updateN text
        , Cmd.none
        )

    ChangeX text ->
        ( model
            |> updateX text
        , Cmd.none
        )

-- subscription

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none






singleObservationLayout n pNull pTruth x nErr pNullErr pTruthErr xErr =
  Form.form []
    [ h4 [] [ Html.text "Simulation Setup"]
    , Html.br [] []
    , Form.group []
        [
          Grid.row []
            [ Grid.col [ Col.xs5 ]
                [ n ]
            ]
        , Grid.row []
            [ Grid.col [ Col.xs5 ]
                      [ pNull ]
            ]

        , Grid.row []
            [ Grid.col [ Col.xs5 ]
                      [ pTruth ]
            ]

        , Grid.row []
            [ Grid.col [ Col.xs5 ]
                      [ x ]
            ]
        ]
    , nErr 
    , pNullErr 
    , pTruthErr 
    , xErr 
    ]

-- view entry point for main app
singleObservationView model =
    singleObservationLayout
       (nEntry ChangeN model.nData.state)
       (pNullEntry ChangePNull model.pNull.state)
       (pTruthEntry ChangePTruth model.pTruth.state)
       (xEntry ChangeX model.xData.state)
       (nError model)
       (pNullError model)
       (pTruthError model)
       (xError model)


-- view for debug

exampleSingleObservationView =
  let
    state = 
      { successLbl = "Correct"
      , failureLbl = "Incorrect"
      , p = 0.25
      , n = 20
      , statistic = "Count"
      }
  in
    singleObservationLayout
        (Html.text ("Success: " ++ state.successLbl))
        (Html.text ("Failure: " ++ state.failureLbl))
        (Html.text ("p: " ++ (String.fromFloat state.p)))
        (Html.text ("n: " ++ (String.fromFloat state.n)))
        (Html.text ("Statistic: " ++ state.statistic))
        (Html.text "")
        (Html.text "")
        (Html.text "")


debugView model =
    div [] 
            [ model.successLbl |> Debug.toString |> makeHtmlText "Success: "
            , Html.br [][]
            , model.failureLbl |> Debug.toString |> makeHtmlText "Failure: "
            , Html.br [][]
            , model.pData |> Debug.toString |> makeHtmlText "pData: "
            , Html.br [][]
            , model.nData |> Debug.toString |> makeHtmlText "nData: "
            , Html.br [][]
            ]

mainGrid model =
    div [] [ Grid.container []
                [ Grid.row []
                    [ Grid.col [ Col.md4, Col.sm4 ] [sidebar model]
                    , Grid.col [ Col.md8, Col.sm8] [div [id "vis"][]]
                    ]
                ]
            ]


sidebar model =
   div [] [ h3 [] [Html.text "Exact Binomial Probability"]
          , nEntry ChangeN model
          , nError model
          , inputGroup "probability" "0.5" ChangeP "p = "
          , outputVal model.p
          , br [] [] 
          , ButtonGroup.radioButtonGroup []
                  [ ButtonGroup.radioButton
                          (model.tail == Left)
                          [ Button.primary, Button.onClick <| ChangeTail Left ]
                          [ Html.text "Left-tail" ]
                  , ButtonGroup.radioButton
                          (model.tail == Right)
                          [ Button.primary, Button.onClick <| ChangeTail Right ]
                          [ Html.text "Right-tail" ]
                  , ButtonGroup.radioButton
                          (model.tail == Two)
                          [ Button.primary, Button.onClick <| ChangeTail Two ]
                          [ Html.text "Two-tail" ]
                  ]
          , inputX model
          , outputX model.xMsg
          , br [] [] 
          , br [] [] 
          , h4 [] [Html.text "Probability"]
          , displayXProb model
          ]


-- main view for subpage debug

view : Model -> Html Msg
view model =
    mainGrid model
