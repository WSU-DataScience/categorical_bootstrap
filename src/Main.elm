port module Main exposing (..)

import Browser
import Task
import Debug
import Random
import Binomial exposing (getBinomGen, roundFloat)
import CollectButtons exposing (collectButtons, totalCollectedTxt, resetButton) 
import CountDict exposing (CountDict, updateCountDict)
import DataSet exposing (Sample, DataSet, LabelFreq, getLabels, createDataFromRegular, createDataFromFreq, getNewData, getSuccess, updateSample, makeEmptySample)
import Defaults exposing (defaults)
import ReadCSV exposing (Variable, example2, getHeadTail, getVariables)
import Limits exposing (Tail(..), TailBound, TailValue(..))
import DistPlot exposing (DistPlotConfig, distPlot, getMaxHeight, getTailExpression, getXandY)
import SamplePlot exposing (samplePlot)
import Dict 
import File exposing (File, name)
import File.Select as Select
import Html exposing (Html, button, p, text, div, h3, h5, h4, b, br)
import Html.Attributes exposing (style, value, class, id)
import Html.Events exposing (onClick)
import List.Extra exposing (zip, group, last, scanl, takeWhile, dropWhileRight)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Dropdown as Dropdown
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Modal as Modal
import Bootstrap.Table as Table
import VegaLite exposing (Spec, bar, circle)


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

type alias SampleData = { p : Float
                        , successLbl : String
                        , failureLbl : String
                        , ws : List Float
                        }

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


initModel : Model
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

updateBinomGen : Sample -> (Float -> Int)
updateBinomGen sample =
    let
        n = sample.n
        nF = n |> toFloat
        successes = sample.numSuccess |> toFloat
        p = successes / nF
    in
        getBinomGen n p
    

updateYs : List Float -> Model -> Model
updateYs ws model =
    let
        (newYs, newTrials) = 
            case model.binomGen of
                Nothing ->
                    (model.ys, model.trials)
            
                Just binomGen -> 
                    (ws |> updateCountDict binomGen model.ys
                    , model.trials + List.length ws
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

sampleConfig : Model -> Sample -> DistPlotConfig
sampleConfig model sample =
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
        in
            { tailExpression = tailExpr
            , xs = ps
            , ys = ys
            , maxY = getMaxHeight model.ys
            , minX = Basics.max 0 (smallestX/n)
            , maxX = Basics.min 1 (largestX/n)
            , mark = if isLarge then bar else circle
            , xAxisTitle = "Proportion of " ++ sample.successLbl
            }

updateDistPlotConfig : Model -> Model
updateDistPlotConfig model =
    case model.originalSample of
        Nothing ->
            model

        Just sample ->
            { model | distPlotConfig = sample |> sampleConfig model |> Just}

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

oneTailBound : (Float -> List (Float, Float) -> Maybe Float) -> (Float -> TailValue Float) -> Model -> Float -> Sample -> TailValue Float
oneTailBound boundFunc tailFunc model level sample =
    let
        tailArea = 1 - level
    in
        model.ys
        |> getPercentiles sample.n model.trials
        |> boundFunc tailArea
        |> Maybe.map tailFunc
        |> Maybe.withDefault NoBounds

getLeftTailBound : Model -> Float -> Sample -> TailValue Float

getLeftTailBound = oneTailBound leftTailBound Lower



getRightTailBound : Model -> Float -> Sample -> TailValue Float
getRightTailBound = oneTailBound rightTailBound Upper



getTwoTailBound : Model -> Float -> Sample -> TailValue Float
getTwoTailBound model level sample = 
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


getBound : Model -> TailBound
getBound model =
    case (model.originalSample, model.tail, model.level) of
        (Just sample, Left, Just level) -> 
            getLeftTailBound model level sample


        (Just sample, Right, Just level) -> 
            getRightTailBound model level sample

        (Just sample, Two, Just level) -> 
            getTwoTailBound model level sample
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
                    |> Tuple.mapSecond List.tail
                    |> Tuple.mapSecond (Maybe.withDefault [])
                    |> Tuple.mapSecond List.sort
                    |> Tuple.mapSecond group
                    |> Tuple.mapSecond (List.map (Tuple.mapSecond (List.length >> (+) 1)))
                    |> Debug.toString
                    |> text
                , br [] []
                , br [] []
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

dropdownItem : (String -> Msg) -> String -> Dropdown.DropdownItem Msg
dropdownItem msg txt =
    Dropdown.buttonItem [ onClick (txt |> msg),  style "width" "max-content !important" ] [ Html.text txt ]


dropdownDataItem : String -> Dropdown.DropdownItem Msg
dropdownDataItem = dropdownItem ChangeData

dropdownSuccessItem : String -> Dropdown.DropdownItem Msg
dropdownSuccessItem = dropdownItem ChangeSuccess


dropdownData : Model -> List (Dropdown.DropdownItem Msg)
dropdownData model =
    model.datasets
    |> List.map .name
    |> List.map dropdownDataItem

dropdownSuccess : Model -> List (Dropdown.DropdownItem Msg)
dropdownSuccess model =
    case model.selected of
        Nothing ->
            []
        
        Just d ->
            d
            |> getLabels
            |> List.map dropdownSuccessItem


dataPlaceholder : Model -> String
dataPlaceholder model =
    case model.selected of
        Nothing ->
            "Select"

        Just data ->
            data.name

dataButtonToggle : Model -> Dropdown.DropdownToggle msg
dataButtonToggle _ = 
    Dropdown.toggle [ Button.outlinePrimary -- pulldownOutline model
                    , Button.small 
                    ] 
                    []

noSelectedVar : Model -> Bool
noSelectedVar model =
    case model.selected of
        Nothing ->
            True
        
        _ ->
            False

successButtonToggle : Model -> Dropdown.DropdownToggle msg
successButtonToggle model =
    let
        attr = [ Button.outlinePrimary , Button.small, model |> noSelectedVar |> Button.disabled ]
    in
        Dropdown.toggle  attr []

successPlaceholder : Model -> String
successPlaceholder model =
    case model.originalSample of
        Nothing ->
            "Select"
    
        Just s ->
            s.successLbl


variableItem : String -> Select.Item msg
variableItem lbl =
   Select.item [ value lbl] [ text lbl]


variableItems : Model -> List (Select.Item msg)
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
    

makeTableRow : LabelFreq -> Table.Row msg
makeTableRow labelFreq =
    Table.tr []
        [ Table.td [] [ labelFreq |> .label |> text  ]
        , Table.td [] [ labelFreq |> .count |> String.fromInt |> text  ]
        ]

genericDropdown : (Model -> String) -> String -> (Model -> Dropdown.State) -> (Dropdown.State -> Msg) -> (Model -> Dropdown.DropdownToggle Msg) -> (Model -> List (Dropdown.DropdownItem Msg)) -> Model -> Html Msg
genericDropdown placeholder lbl state msg toggle items model =
    InputGroup.config
        ( InputGroup.text [ Input.placeholder (placeholder model), Input.disabled True] )  --++ inputFeedback model) )
        |> InputGroup.small
        |> InputGroup.predecessors
            [ InputGroup.span [] [ b [] [text lbl]] ]
        |> InputGroup.successors
                [ InputGroup.dropdown
                    (model |> state)
                    { options = [ Dropdown.alignMenuRight ]
                    , toggleMsg = msg
                    , toggleButton = toggle model
                    , items = items model
                    }
                ]
        |> InputGroup.view

dataDropdown : Model -> Html Msg
dataDropdown = genericDropdown dataPlaceholder "Data" .dataDropState DataDropMsg dataButtonToggle dropdownData
    

successDropdown : Model -> Html Msg
successDropdown = genericDropdown successPlaceholder "Success" .successDropState SuccessDropMsg successButtonToggle dropdownSuccess



noPerspectiveData : Model -> Bool
noPerspectiveData model =
    model.perspectiveData == Nothing

genericButton : (Model -> Bool) -> List (Html.Attribute Msg) -> Msg -> String -> Model -> Html Msg
genericButton isDisabled attrs onclick lbl model =
    Button.button
        [ Button.small
        , Button.primary
        , Button.disabled (model |> isDisabled)
        , Button.attrs attrs
        , Button.onClick onclick 
        ]
        [ text lbl ]

uploadButton : Model -> Html Msg
uploadButton = genericButton (\_ -> False) [ Spacing.ml1 ] ShowModal "Upload CSV"


csvFileButton : Model -> Html Msg
csvFileButton = genericButton (\_ -> False) [ Spacing.ml1 ] CsvRequested "CSV File"


loadFileButton : Model -> Html Msg
loadFileButton = genericButton noPerspectiveData [  class "float-right" ] LoadUserData "Load"


fileTypeItems : Model -> List (Select.Item msg)
fileTypeItems _ =   [ Select.item [ value "none"] [ text "Not Selected"]
                    , Select.item [ value "reg"] [ text "Regular"]
                    , Select.item [ value "freq"] [ text "Frequency"]
                    ]

noTypeSelected : Model -> Bool
noTypeSelected model = 
    model.fileType == NoType

noFileName : Model -> Bool
noFileName model =
    model.fileName == "None"

notFrequency : Model -> Bool
notFrequency model =
    model.fileType /= Frequency



genericSelect : String -> (Model -> Bool) -> (String -> Msg) -> (Model -> List (Select.Item Msg)) -> Model -> Html Msg
genericSelect id isDisabled onchange items model =
    Select.custom 
        [ Select.disabled (model |> isDisabled)
        , Select.id id
        , Select.onChange onchange
        ]
        (items model)

fileTypeSelect : Model -> Html Msg
fileTypeSelect = genericSelect "fileselect"  noFileName ChangeFileType fileTypeItems


variableSelect : Model -> Html Msg
variableSelect = genericSelect "varselect"  noTypeSelected SelectVariable variableItems


countSelect : Model -> Html Msg
countSelect = genericSelect "countselect" notFrequency SelectCount variableItems


previewTable : Model -> Html Msg
previewTable model = 
    case model.perspectiveData of
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

uploadFormRow : Model -> String -> (Model -> Html Msg) -> Html Msg
uploadFormRow model lbl input =
    Form.row []
        [ Form.colLabel [ Col.sm4 ] [ text lbl ]
        , Form.col [ Col.sm6 ] [ input model ]
        ]

uploadRowsRaw : List (String, Model -> Html Msg)
uploadRowsRaw = [ ("Select File", csvFileButton)
                , ("File Name", \model -> text model.fileName )
                , ("File Type", fileTypeSelect)
                , ("Variable", variableSelect)
                , ("Counts", countSelect)
                , ("Preview", previewTable)
                , ("", loadFileButton)
                ]  

uploadForm : Model -> Html Msg
uploadForm model =
    let
        lbls = List.map Tuple.first uploadRowsRaw
        funcs = List.map Tuple.second uploadRowsRaw
    in
    
    Grid.container []
                    [ Form.form []
                      (List.map2 (uploadFormRow model) lbls funcs)
                    ]

dataEntryInputGroupView : Model -> Html Msg
dataEntryInputGroupView model =
    div []
        [ Html.h4 [] [Html.text "Data Selection"]
        , model |> dataDropdown
        , model |> successDropdown
        , uploadButton model
        , Modal.config CloseModal
            |> Modal.h5 [] [ text "Upload CSV data" ]
            |> Modal.body [] [ uploadForm model ]
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


statisticView : (Model -> Maybe Sample) -> Model -> Html Msg
statisticView dotSample model =
    case model |> dotSample of
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


sampleGrid : (Model -> Maybe Sample) -> String -> String -> Model -> Html Msg
sampleGrid dotSample label plotId model =
    case model |> dotSample of
            Nothing ->
                div [] []
            _ ->
                div []
                    [ Form.group []
                        [ h4 [] [ Html.text label]
                        , Grid.row []
                            [ Grid.col [ Col.xs7 ] [ div [id plotId ] [] ]
                            , Grid.col [ Col.xs5 ] [ statisticView dotSample model ]
                            ]
                        ]
                    ]

originalSampleView : Model -> Html Msg
originalSampleView = sampleGrid .originalSample "Original Sample" "samplePlot"


bootstrapSampleView : Model -> Html Msg
bootstrapSampleView = sampleGrid .bootstrapSample "Bootstrap Sample" "bootstrapPlot"

collectButtonGrid reset buttons count =
    div [] [ Form.group []
              [ Grid.row []
                  [ Grid.col  [ Col.xs9 ]
                              [ Html.h4 [] [Html.text "Collect Statistics"]
                              ]
                  , Grid.col  [ Col.xs3 ]
                              [ reset ]
                              
                  ]
              , Grid.row []
                  [ Grid.col  [ Col.xs10 ]
                              [ buttons ]
                              
                  ]
              , Grid.row []
                  [  Grid.col  [ Col.xs10 ]
                              [ count ]
                  ]
              ]
          ]

collectButtonView : Model -> Html Msg
collectButtonView model =
    collectButtonGrid 
        ( resetButton Reset)
        ( if model.originalSample /= Nothing then 
            collectButtons Collect defaults.collectNs 
          else 
            div [] [])
        ( totalCollectedTxt model.trials )


distPlotView : Model -> Html Msg
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


confLimitsView : Model -> Html Msg
confLimitsView model =
    div [] [ Form.group []
              [ Grid.row []
                  [ Grid.col  [ Col.xs9 ]
                              [ Html.h4 [] [Html.text "Confidence Interval"]
                              , div []
                                    [ Grid.container []
                                            [ Grid.row []
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
                              
                              ]
                    ]
              ]
    ]



tailButtonView : Model -> Html Msg
tailButtonView model =
  ButtonGroup.radioButtonGroup []
          [ ButtonGroup.radioButton
                  (model.tail == Left)
                  [ Button.primary, Button.small, Button.onClick <| ChangeTail Left ]
                  [ Html.text "Left" ]
          , ButtonGroup.radioButton
                  (model.tail == Right)
                  [ Button.primary, Button.small, Button.onClick <| ChangeTail Right ]
                  [ Html.text "Right" ]
          , ButtonGroup.radioButton
                  (model.tail == Two)
                  [ Button.primary, Button.small, Button.onClick <| ChangeTail Two ]
                  [ Html.text "Two" ]
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
                    [ Grid.col  [ Col.md4 ]
                                [ dataEntry]
                    , Grid.col [ Col.md4]
                                [ originalSample ]
                    , Grid.col [ Col.md4]
                               [ bootstrapSample ]
                    ]
                , Grid.row []
                    [ Grid.col [ Col.md4]
                               [div [] 
                                    [ collectButtons
                                    , br [] []
                                    , confLimits

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


samplePlotCmd : (Spec -> Cmd.Cmd msg) -> (Model -> Maybe Sample) -> Model -> Cmd.Cmd msg
samplePlotCmd cmd dotSample model =
    case model |> dotSample of
        Nothing ->
            Cmd.none

        Just sample ->
            cmd (samplePlot sample)


originalPlotCmd : Model -> Cmd.Cmd msg
originalPlotCmd = samplePlotCmd samplePlotToJS .originalSample


bootstrapPlotCmd : Model -> Cmd.Cmd msg
bootstrapPlotCmd = samplePlotCmd bootstrapPlotToJS .bootstrapSample

distPlotCmd : Model -> Cmd.Cmd msg
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