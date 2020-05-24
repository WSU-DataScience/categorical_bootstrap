port module Main exposing (..)

import Browser
import Task
import Debug
import Random
import Binomial exposing (..)
import DataSet exposing (..)
import DataEntry exposing (..)
import CountDict exposing (..)
import Dict exposing (..)
import File exposing (File, name)
import File.Select as Select
import Html exposing (Html, button, p, text, div, h2, h3, h5, h4, b, br)
import Html.Attributes exposing (style, for, value, class, placeholder, id)
import Html.Events exposing (onClick)
import List.Extra exposing (zip, getAt, group, last, scanl, takeWhile, dropWhileRight)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Dropdown as Dropdown
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Modal as Modal
import OneSample exposing (SampleData)
import DataSet exposing (..)
import ReadCSV exposing (..)
import Bootstrap.Table as Table
import SamplePlot exposing (..)
import VegaLite exposing (Spec, bar, circle)
import Defaults exposing (defaults)
import CollectButtons exposing (collectButtons, totalCollectedTxt, resetButton) 
import Layout exposing (collectButtonGrid)
import Limits exposing (..)
import DistPlot exposing (..)


-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type FileType = NoType | Regular | Frequency

type alias Model =
    { datasets : List DataSet
    , csv : Maybe String
    , dataDropState : Dropdown.State
    , successDropState : Dropdown.State
    , selected : Maybe DataSet
    , originalSample : Maybe Sample
    , bootstrapSample : Maybe Sample
    , modalVisibility : Modal.Visibility
    , fileName : String
    , fileType : FileType
    , variables : Maybe (List Variable)
    , selectedVariable : Maybe String
    , selectedData : Maybe (List String)
    , counts : Maybe (List Int)
    , perspectiveData : Maybe DataSet
    , binomGen : Maybe (Float -> Int)
    , trials : Int
    , ys : CountDict
    , isProportion : Bool
    , tailLimit : TailBound
    , tail : Tail
    , distPlotConfig : Maybe DistPlotConfig
    , level : Maybe Float
    }


initModel = { datasets = DataSet.datasets
            , csv = Nothing
            , dataDropState = Dropdown.initialState
            , successDropState = Dropdown.initialState
            , selected = Nothing
            , originalSample = Nothing
            , bootstrapSample = Nothing
            , modalVisibility = Modal.hidden
            , fileName = "None"
            , fileType = NoType
            , variables = Nothing
            , selectedVariable = Nothing
            , selectedData = Nothing
            , counts = Nothing
            , perspectiveData = Nothing
            , binomGen = Nothing
            , trials = 0
            , ys = Dict.empty
            , isProportion = True
            , tailLimit = NoBounds
            , tail = None
            , distPlotConfig = Nothing
            , level = Nothing
            }


init : () -> (Model, Cmd Msg)
init _ =
  ( initModel, Cmd.none )



-- UPDATE


type Msg
    = CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | DataDropMsg Dropdown.State
    | SuccessDropMsg Dropdown.State
    | ChangeData String
    | ChangeSuccess String
    | CloseModal
    | ShowModal
    | ChangeFileType String
    | SelectVariable String
    | SelectCount String
    | LoadUserData
    | Collect Int
    | NewStatistics (List Float)
    | Reset
    | ChangeTail Tail
    | ChangeLevel Float


-- update

updateBinomGen sample =
    let
        n = sample.n
        p = (toFloat sample.numSuccess)/(toFloat n)
    in
        getBinomGen n p
    

updateYs ws model =
    let
        (newYs, newTrials) = 
            case model.binomGen of
                Nothing ->
                    (model.ys, model.trials)
            
                Just binomGen -> 
                    (ws |> updateCountDict binomGen model.ys
                    , model.trials + (List.length ws)
                    )

    in
        { model | ys = newYs 
                , trials = newTrials
                }

apply : (a -> b) -> a -> b
apply f x =
    f x

updateLast : List Float -> Model -> Model
updateLast ws model =
  let
    lastX = Maybe.map2 apply model.binomGen (last ws)
    newBootstrap = Maybe.map2 updateSample lastX model.bootstrapSample
  in
    { model | bootstrapSample = newBootstrap }

updateOriginal : Maybe Sample -> Model -> Model
updateOriginal newSample model =
    { model |  originalSample = newSample }


resetBootstrap : Maybe Sample -> Model -> Model
resetBootstrap sample model =
    { model |  bootstrapSample = sample |> Maybe.map makeEmptySample }


updateBootstrap : Int -> Model -> Model
updateBootstrap numSuccesses model =
    { model | bootstrapSample = model.bootstrapSample |> Maybe.map (updateSample numSuccesses)}


updateBinom : Maybe Sample -> Model -> Model
updateBinom newSample model =
    { model | binomGen = newSample |> Maybe.map updateBinomGen }


resetTrials : Model -> Model
resetTrials model =
    { model | trials = 0
            , ys = Dict.empty
            , level = Nothing
            , tail = None
            , tailLimit = NoBounds
            }


updateDistPlotConfig : Model -> Model
updateDistPlotConfig model =
    case model.originalSample of
        Nothing ->
            model

        Just sample ->
            let
                n = sample.n |> toFloat
                p = (toFloat sample.numSuccess)/n
                mean = n*p
                tailExpr = getTailExpression mean model.tailLimit
                isLarge = model.trials > defaults.largePlot
                (xs, ys) = getXandY isLarge model.ys
                smallestX = Maybe.withDefault 0 (List.minimum xs) - 3
                largestX = Maybe.withDefault n (List.maximum xs) + 3 
                ps = xs |> List.map (\x -> x/n)
                numSD = defaults.numSD
                sd = p*(1-p)/n
                config = Just { tailExpression = tailExpr
                            , xs = ps
                            , ys = ys
                            , maxY = getMaxHeight model.ys
                            , minX = Basics.max 0 (smallestX/n)
                            , maxX = Basics.min 1 (largestX/n)
                            , mark = if isLarge then bar else circle
                            , xAxisTitle = "Proportion of " ++ sample.successLbl
                            }
            in
                { model | distPlotConfig = config}

divideBy : Int -> Int -> Float
divideBy denom numer =
    (toFloat numer)/(toFloat denom) 

getPercentiles : Int -> Int -> CountDict -> List (Float, Float)
getPercentiles n trials counts =
        counts
        |> Dict.toList
        |> List.map (Tuple.mapBoth (divideBy n) (divideBy trials))
        |> \el -> zip (List.map Tuple.first el) (el |> List.map Tuple.second |> scanl (+) 0 )

leftTailBound : Float -> List (Float, Float) -> Maybe Float
leftTailBound tailArea percentiles =
    percentiles
    |> takeWhile (Tuple.second >> (>) tailArea)
    |> last
    |> Maybe.map Tuple.first

rightTailBound : Float -> List (Float, Float) -> Maybe Float
rightTailBound tailArea percentiles =
    let
        oneMinus = 1 - tailArea
    in
        percentiles
        |> dropWhileRight (Tuple.second >> (<) oneMinus)
        |> last
        |> Maybe.map Tuple.first

getBound : Model -> TailBound
getBound model =
    case (model.originalSample, model.tail, model.level) of
        (Just sample, Left, Just level) -> 
            let
                tailArea = 1 - level
            in
                model.ys
                |> getPercentiles sample.n model.trials
                |> leftTailBound tailArea
                |> Maybe.map Lower
                |> Maybe.withDefault NoBounds


        (Just sample, Right, Just level) -> 
            let
                tailArea = 1 - level
            in
                model.ys
                |> getPercentiles sample.n model.trials
                |> rightTailBound tailArea
                |> Maybe.map Upper
                |> Maybe.withDefault NoBounds

        (Just sample, Two, Just level) -> 
            let
                tailArea = (1 - level)/2
                percentiles =
                    model.ys
                    |> getPercentiles sample.n model.trials
                left = percentiles
                        |> leftTailBound tailArea
                right = percentiles
                    |> rightTailBound tailArea
            in
                Maybe.map2 TwoTail left right
                |> Maybe.withDefault NoBounds

        _ ->
            NoBounds

updateTailBounds : Model -> Model
updateTailBounds model =
    { model | tailLimit = getBound model}


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CsvRequested ->
      (  { model | fileName = "None"
                 , fileType = NoType
                 , variables = Nothing
                 , selectedData = Nothing
                 , selectedVariable = Nothing
                 , counts = Nothing
         }
      , Select.file ["text/csv"] CsvSelected
      )

    CsvSelected file ->
      ( { model | fileName = name file }
      , Task.perform CsvLoaded (File.toString file)
      )

    CsvLoaded content ->
      ( { model | csv = Just content , variables = getVariables content}
      , Cmd.none
      )

    DataDropMsg state ->
        ( { model | dataDropState = state }
        , Cmd.none
        )

    SuccessDropMsg state ->
        ( { model | successDropState = state }
        , Cmd.none
        )

    ChangeData name ->
        let
            data = model.datasets
                    |> getNewData name
            finalModel =
               { model | selected = data , originalSample = Nothing} 
                |> resetTrials

        in
            ( finalModel
            , Cmd.batch [ originalPlotCmd finalModel , bootstrapPlotCmd finalModel, distPlotCmd finalModel] 
            )

    ChangeSuccess name ->
        let
            newSample = model.selected
                        |>  Maybe.andThen (getSuccess name)
            finalModel = 
                model
                |> updateOriginal newSample
                |> resetBootstrap newSample
                |> updateBinom newSample
                |> updateDistPlotConfig
                |> updateTailBounds
        in
            ( finalModel
            , Cmd.batch [ originalPlotCmd finalModel , bootstrapPlotCmd finalModel, distPlotCmd finalModel] 
            )

    CloseModal ->
        let
            finalModel =
                { model | modalVisibility = Modal.hidden }
        in
            ( finalModel
            , Cmd.batch [ originalPlotCmd finalModel , bootstrapPlotCmd finalModel, distPlotCmd finalModel] 
            )

    ShowModal ->
        ( { model | modalVisibility = Modal.shown }
        , Cmd.none
        )

    ChangeFileType s ->
        let
            ftype = 
                case s of
                    "reg" ->
                        Regular

                    "freq" ->
                        Frequency

                    _ ->
                        NoType

        in
            ( { model | fileType = ftype }
                |> resetTrials
            , Cmd.none
            )

    SelectVariable lbl ->
        let
            (var, col) = 
                case model.variables of
                    Nothing ->
                        (Nothing, Nothing)
                    Just vars ->
                        (Just lbl, vars)
                        |> Tuple.mapSecond (List.Extra.find (\p -> Tuple.first p == lbl)) 
                        |> Tuple.mapSecond (Maybe.map Tuple.second)

        in
            ( { model | selectedData = col, selectedVariable = var}
              |> updatePerspective
            , Cmd.none
            )
        
    SelectCount lbl ->
        let
            rawcol = 
                case model.variables of
                    Nothing ->
                        Nothing
                    Just vars ->
                        vars
                        |> List.Extra.find (\p -> Tuple.first p == lbl) 
                        |> Maybe.map Tuple.second
            counts = 
                rawcol
                |> Maybe.map (List.map (String.toInt >> Maybe.withDefault 0))
        in
            ( { model | counts = counts }
              |> updatePerspective
            , Cmd.none
            )

    LoadUserData ->
        case model.perspectiveData of
            Nothing ->
                (model, Cmd.none)
        
            Just data ->
                let
                    finalModel =
                        { model | selected = Just data 
                        , originalSample = Nothing
                        , datasets = data :: model.datasets
                        , modalVisibility = Modal.hidden 
                        } |> resetTrials
                in
                ( finalModel
                , Cmd.batch [ originalPlotCmd finalModel , bootstrapPlotCmd finalModel, distPlotCmd finalModel] 
                )

    Collect n ->
        ( model
        , Random.generate NewStatistics (Random.list n (Random.float 0 1))
        )

    
    Reset ->
        let
            finalModel = 
                model
                |> resetBootstrap model.originalSample
                |> resetTrials
                |> updateDistPlotConfig
        in
        
        (finalModel
        , Cmd.batch [ originalPlotCmd finalModel , bootstrapPlotCmd finalModel, distPlotCmd finalModel] 
        )


    
    NewStatistics ws ->
        let
            finalModel =
                model 
                |> updateYs ws
                |> updateLast ws
                |> updateDistPlotConfig
                |> updateTailBounds
        in
        
        ( finalModel 
        , Cmd.batch [ originalPlotCmd finalModel , bootstrapPlotCmd finalModel, distPlotCmd finalModel] 
        )

    ChangeTail tail ->
        let
            finalModel =
                { model | tail = tail }
                        |> updateTailBounds
                        |> updateDistPlotConfig
        in
        ( finalModel
        , Cmd.batch [ originalPlotCmd finalModel , bootstrapPlotCmd finalModel, distPlotCmd finalModel] 
        )
                
        
    ChangeLevel level ->
        let
            finalModel =
                  { model | level = Just level }
                        |> updateTailBounds
                        |> updateDistPlotConfig
        in
        ( finalModel
        , Cmd.batch [ originalPlotCmd finalModel , bootstrapPlotCmd finalModel, distPlotCmd finalModel] 
        )



createDataFromUser : Model -> Maybe DataSet
createDataFromUser model =
    case (model.fileType, (model.selectedVariable, model.selectedData), model.counts) of
        (Regular, (Just var, Just data), _) ->
            Just (createDataFromRegular (var ++ "--" ++ model.fileName) data)
    
        (Frequency, (Just var, Just lbls), Just cnts) ->
            Just (createDataFromFreq (var ++ "--" ++ model.fileName) lbls cnts)

        _ ->
            Nothing

updatePerspective : Model -> Model
updatePerspective model =
    { model |  perspectiveData = createDataFromUser model}
            
    

-- SUBSCRIPTION

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.dataDropState DataDropMsg
        , Dropdown.subscriptions model.successDropState SuccessDropMsg
        ]

-- VIEW)

fileContent : Model -> Html Msg
fileContent model = 
    let
        n = 1000
        trials = model.trials
        percentiles = model.ys |> getPercentiles n trials
        
    in
    
  case model.csv of
    Nothing ->
      div []    [ percentiles
                    |> leftTailBound 0.05
                    |> Debug.toString
                    |> text
                , percentiles
                    |> rightTailBound 0.05
                    |> Debug.toString
                    |> text

                , percentiles
                    |> takeWhile (Tuple.second >> (>) 0.05)
                    |> last
                    |> Maybe.map Tuple.first
                    |> Debug.toString
                    |> text
                , percentiles
                    |> dropWhileRight (Tuple.second >> (<) 0.95)
                    |> last
                    |> Maybe.map Tuple.first
                    |> Debug.toString
                    |> text
                , model
                    |> Debug.toString
                    |> text
                , br [] []
                , br [] []
                , example2
                    |> String.trim
                    |> getHeadTail
                    |> Maybe.withDefault ("guh", [])
                    -- |> Tuple.mapSecond (String.split "\n")
                    |> Tuple.mapSecond (List.tail)
                    |> Tuple.mapSecond (Maybe.withDefault [])
                    |> Tuple.mapSecond (List.sort)
                    |> Tuple.mapSecond (group)
                    |> Tuple.mapSecond (List.map (Tuple.mapSecond (List.length >> (+) 1)))
                    |> Debug.toString
                    |> text
                , br [] []
                , br [] []
             ]
    Just content ->
        let
            rows = content
                |> String.split "\n"
                |> List.filter (String.isEmpty >> not)
                |> List.map (String.split ",")
            header = Maybe.withDefault [] (rows |> List.head)
            body = 
               case List.tail rows of 
                Nothing ->
                    []
                Just el ->
                    el
        in
        
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


dropdownItem msg txt =
    Dropdown.buttonItem [ onClick (txt |> msg) ] [ Html.text txt ]

dropdownDataItem = dropdownItem ChangeData

dropdownSuccessItem = dropdownItem ChangeSuccess


dropdownData datasets =
    datasets
    |> List.map .name
    |> List.map dropdownDataItem

dropdownSuccess model =
    case model.selected of
        Nothing ->
            []
        
        Just d ->
            d
            |> getLabels
            |> List.map dropdownSuccessItem


dataPlaceholder model =
    case model.selected of
        Nothing ->
            "Select"

        Just data ->
            data.name


successButtonToggle model =
    let
        disabled =
            case model.selected of
                Nothing ->
                    True
                
                _ ->
                    False
        attr = [ Button.outlinePrimary , Button.small, Button.disabled disabled]
    in
        Dropdown.toggle  attr []

successPlaceholder model =
    case model.originalSample of
        Nothing ->
            "Select"
    
        Just s ->
            s.successLbl


variableItem lbl =
   Select.item [ value lbl] [ text lbl]


variableItems model =
    let
        baseItems = [ Select.item [ value "none"] [ text "Not Selected"]]
        varItems = 
            case model.variables of
                Nothing ->
                    []
                Just el ->
                    let
                        varNames = List.map Tuple.first el
                    in
                        List.map variableItem varNames
    in
        baseItems ++ varItems
    

makeTableRow labelFreq =
    Table.tr []
        [ Table.td [] [ labelFreq |> .label |> text  ]
        , Table.td [] [ labelFreq |> .count |> String.fromInt |> text  ]
        ]

    

dataEntryInputGroupView model =
    div []
        [ Html.h4 [] [Html.text "Data Selection"]
        , InputGroup.config
            ( InputGroup.text [ Input.placeholder (dataPlaceholder model), Input.disabled True] )  --++ inputFeedback model) )
            |> InputGroup.small
            |> InputGroup.predecessors
                [ InputGroup.span [] [ b [] [text "Data"]] ]
            |> InputGroup.successors
                    [ InputGroup.dropdown
                        model.dataDropState
                        { options = [ Dropdown.alignMenuRight ]
                        , toggleMsg = DataDropMsg
                        , toggleButton = 
                            Dropdown.toggle [ Button.outlinePrimary -- pulldownOutline model
                                            , Button.small 
                                            ] 
                                            []
                        , items = dropdownData model.datasets 
                        }
                    ]
            |> InputGroup.view
        , InputGroup.config
            ( InputGroup.text [ Input.placeholder (successPlaceholder model), Input.disabled True])  -- ++ inputFeedback model) )
            |> InputGroup.small
            |> InputGroup.predecessors
                [ InputGroup.span [] [ b [] [text "Success"]] ]
            |> InputGroup.successors
                    [ InputGroup.dropdown
                        model.successDropState
                        { options = [ Dropdown.alignMenuRight ]
                        , toggleMsg = SuccessDropMsg
                        , toggleButton = successButtonToggle model
                        , items = dropdownSuccess model
                        }
                    ]
            |> InputGroup.view
        , Button.button
            [ Button.small, Button.primary, Button.attrs [ Spacing.ml1 ],  Button.onClick ShowModal ]
            [ text "Upload CSV" ]
        , Modal.config CloseModal
            |> Modal.h5 [] [ text "Upload CSV data" ]
            |> Modal.body []
                [ Grid.container []
                    [ Form.form []
                        [ Form.row []
                            [ Form.colLabel [ Col.sm4 ] [ text "Select File" ]
                            , Form.col [ Col.sm4 ]
                                [ Button.button
                                    [ Button.small, Button.primary, Button.attrs [ Spacing.ml1 ],  Button.onClick CsvRequested ]
                                    [ text "CSV File" ]
                                ]
                            ]
                        , Form.row []
                            [ Form.colLabel [ Col.sm4 ] [ text "File Name" ]
                            , Form.col [ Col.sm6 ] [ text model.fileName ]
                            ]
                        , Form.row []
                            [ Form.colLabel [ Col.sm4 ] [ text "File Type" ]
                            , Form.col [ Col.sm6 ]
                                [ Select.custom 
                                    [ Select.disabled (model.fileName == "None")
                                    , Select.id "myselect"
                                    , Select.onChange ChangeFileType
                                    ]
                                    [ Select.item [ value "none"] [ text "Not Selected"]
                                    , Select.item [ value "reg"] [ text "Regular"]
                                    , Select.item [ value "freq"] [ text "Frequency"]
                                    ]
                                ]
                            ]
                        , Form.row []
                            [ Form.colLabel [ Col.sm4 ] [ text "Variable" ]
                            , Form.col [ Col.sm6 ]
                                [ Select.custom 
                                    [ Select.disabled (model.fileType == NoType)
                                    , Select.onChange SelectVariable
                                    ] 
                                    (variableItems model)
                                ]
                            ]
                        , Form.row []
                            [ Form.colLabel [ Col.sm4 ] [ text "Counts" ]
                            , Form.col [ Col.sm6 ]
                                [ Select.custom 
                                    [ Select.disabled (model.fileType /= Frequency)
                                    , Select.onChange SelectCount
                                    ]
                                    (variableItems model)
                                ]
                            ]
                        , case model.perspectiveData of
                            Nothing ->
                                div [] []
                            
                            Just data ->
                                Table.table
                                        { options = [ Table.small, Table.striped, Table.bordered]
                                        , thead = Table.simpleThead
                                                    [ Table.th [] [ text "Label" ]
                                                    , Table.th [] [ text "Count" ]
                                                    ]
                                        , tbody = 
                                            Table.tbody []
                                                        (List.map makeTableRow data.frequencies)
                                        }
                        , Form.row [ Row.rightSm ]
                            [ Form.col [ Col.sm2 ]
                                [ Button.button
                                    [ Button.primary
                                    , Button.attrs [ class "float-right"]
                                    , Button.disabled (model.perspectiveData == Nothing)
                                    , Button.onClick LoadUserData
                                    ]
                                    [ text "Load" ]
                                ]
                            ]
                        ]
                    ]
                ]
            |> Modal.footer []
                [ Button.button
                    [ Button.outlinePrimary
                    , Button.attrs [ onClick CloseModal ]
                    ]
                    [ text "Close" ]
                ]
            |> Modal.view model.modalVisibility
        , br [] [] 
        ]


statisticView maybeSuccess =
    case maybeSuccess of
        Nothing ->
            div [] []
        
        Just success ->
            let
                lbl = success.successLbl
                n = success.n |> String.fromInt
                cnt = success.numSuccess |> String.fromInt
                prop = 
                    (toFloat success.numSuccess )/(toFloat success.n) 
                    |> roundFloat 3
                    |> String.fromFloat 
                emptySample = (success.numSuccess + success.numFailures) == 0
            in
                if emptySample then div [] [] else
                    div [ style "font-size" "smaller"] 
                        [ "N = " ++ n |> text
                        , br [] []
                        , "Count(" ++ lbl ++ ") = " ++ cnt |> text
                        , br [] []
                        , "Prop(" ++ lbl ++ ") = " ++ prop |> text
                        , br [] []
                        ]


sampleGrid label statistics plotDiv =
  div []
      [ Form.group []
        [ h4 [] [ Html.text label]
        , Grid.row []
            [ Grid.col [ Col.xs10 ] [ statistics ]
            , Grid.col [ Col.xs2 ] []
            ]
        , Grid.row []
            [ Grid.col [ Col.xs12]
                       [ plotDiv ]
            ]
        ]
      ]

originalSampleView : Model -> Html msg

originalSampleView model =
    case model.originalSample of
            Nothing ->
                div [] []
            _ ->
                sampleGrid 
                    "Original Sample" 
                    (statisticView model.originalSample)
                    (div [id "samplePlot"] [])


bootstrapSampleView model =
    case model.originalSample of
            Nothing ->
                div [] []
            _ ->
                sampleGrid 
                    "Bootstrap Sample" 
                    (statisticView model.bootstrapSample)
                    (div [id "bootstrapPlot"] [])


collectButtonView model =
    collectButtonGrid 
        ( resetButton Reset)
        ( if model.originalSample /= Nothing then 
            collectButtons Collect defaults.collectNs 
          else 
            div [] [])
        ( totalCollectedTxt model.trials )


distPlotView model =
    if model.originalSample == Nothing  then
            div [] []
    else
            div [id "distPlot" ] []


outputText : Model -> String
outputText model =
    case model.tailLimit of
        NoBounds ->
            "??"

        Lower l ->
            "p > " ++ (l |> roundFloat 4 |> String.fromFloat )
        
        Upper u ->
            "p < " ++ (u |> roundFloat 4 |> String.fromFloat )

        TwoTail l u ->
            (l |> roundFloat 4 |> String.fromFloat ) ++ " < p < " ++ (u |> roundFloat 4 |> String.fromFloat )
        


    


confLimitsView model =
    div []
        [ Grid.container []
                [ Html.h4 [] [Html.text "Confidence Interval"]
                , Grid.row []
                    [ Grid.col [ Col.sm3 ] [ b [] [text "Tail" ]]
                    , Grid.col [ Col.sm8 ] [  tailButtonView model ]
                    , Grid.colBreak []
                    , Grid.col [ Col.sm3 ] [ b [] [text "Level" ]]
                    , Grid.col [ Col.sm8 ] [  levelButtonView model]
                    , Grid.colBreak []
                    , Grid.col [ Col.sm3 ] [ b [] [text "Interval" ]]
                    , Grid.col [ Col.sm8 ] [ model |> outputText |> text ]
                    ]
                ]
        ]



tailButtonView : Model -> Html Msg
tailButtonView model =
  ButtonGroup.radioButtonGroup []
          [ ButtonGroup.radioButton
                  (model.tail == Left)
                  [ Button.primary, Button.small, Button.onClick <| ChangeTail Left ]
                  [ Html.text "Left-tail" ]
          , ButtonGroup.radioButton
                  (model.tail == Right)
                  [ Button.primary, Button.small, Button.onClick <| ChangeTail Right ]
                  [ Html.text "Right-tail" ]
          , ButtonGroup.radioButton
                  (model.tail == Two)
                  [ Button.primary, Button.small, Button.onClick <| ChangeTail Two ]
                  [ Html.text "Two-tail" ]
          ]


makeLevelButton : Model -> Float -> String -> ButtonGroup.RadioButtonItem Msg
makeLevelButton model conf txt =
            ButtonGroup.radioButton
                (case model.level of
                    Nothing ->
                        False
                    Just lvl ->
                        lvl == conf
                )
                [ Button.primary, Button.small, Button.onClick <| ChangeLevel conf ]
                [ Html.text txt ]


levelButtonView : Model -> Html Msg
levelButtonView model =
  ButtonGroup.radioButtonGroup []
          (List.map2 (makeLevelButton model) defaults.levels defaults.levelsTxt)


mainGrid dataEntry originalSample bootstrapSample collectButtons distPlot confLimits debug =
    div [] [  Grid.container []
                [CDN.stylesheet -- creates an inline style node 
                , Grid.row []
                    [ Grid.col  [ Col.md12] 
                                [ h3 [] [text "Bootstrap Confidence Interval for Categorical Data"]
                                , br [] [] 
                                ]
                    ]
                , Grid.row []
                    [ Grid.col  [ Col.md3] 
                                [ dataEntry]
                    , Grid.col [ Col.md4]
                               [ collectButtons ]
                    , Grid.col [ Col.md5]
                               [ confLimits ]
                    ]
                , Grid.row []
                    [ Grid.col [ Col.md4]
                               [div [] 
                                    [ originalSample
                                    , br [] []
                                    , bootstrapSample
                                    ]
                                ]
                    , Grid.col [ Col.md8] 
                               [ distPlot
                               ]
                    ]
                , Grid.row []
                    [ Grid.col  [ Col.md4] []
                    , Grid.col [ Col.md8]
                               [ debug ]
                    ]
                ]
              ]

view : Model -> Html Msg
view model =
  div []
      [ mainGrid 
            (dataEntryInputGroupView model) 
            (originalSampleView model)
            (bootstrapSampleView model)
            (collectButtonView model)
            (distPlotView model)
            (confLimitsView model)
            (div [] [] )
            --(fileContent model)
      ]


-- port

samplePlotCmd cmd dotSample model =
    case model |> dotSample of
        Nothing ->
            Cmd.none

        Just sample ->
            cmd (samplePlot sample)


originalPlotCmd = samplePlotCmd samplePlotToJS .originalSample


bootstrapPlotCmd = samplePlotCmd bootstrapPlotToJS .bootstrapSample

distPlotCmd model =
    case model.distPlotConfig of
        Nothing ->
            Cmd.none

        Just config -> 
            distPlotToJS (distPlot config)

-- send samplePlot to vega
port samplePlotToJS : Spec -> Cmd msg
port bootstrapPlotToJS : Spec -> Cmd msg
port distPlotToJS : Spec -> Cmd msg