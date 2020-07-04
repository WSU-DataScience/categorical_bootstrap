port module Main exposing (..)

import Browser
import Task
import Random
import CollectButtons exposing (collectButtons, totalCollectedTxt, resetButton) 
import CountDict exposing (CountDict, updateCountDict, meanProp, sdProp)
import DataSet exposing (DataSet, LabelFreq, getLabels, createDataFromRegular, createDataFromFreq, getNewData)
import Sample exposing (Sample, getSuccess, updateSample, makeEmptySample, maybeSampleView, updateBinomGen, sampleConfig, getLeftTailBound, getRightTailBound, getTwoTailBound)
import Defaults exposing (defaults)
import ReadCSV exposing (FileType(..), Variable, getVariables, findVariable, getFileType)
import Limits exposing (Tail(..), TailBound, TailValue(..), confIntOutput)
import DistPlot exposing (DistPlotConfig, distPlot)
import Dropdown exposing (DropdownConfig, genericDropdown)
import SamplePlot exposing (samplePlot)
import Utility exposing (apply, roundFloat)
import Dict 
import File exposing (File, name)
import File.Select as Select
import Html exposing (Html, button, text, div, h3, h5, h4, b, br)
import Html.Attributes exposing (style, value, class, id)
import Html.Events exposing (onClick)
import List.Extra exposing (group, last)
import Maybe exposing (withDefault)
import CountDict exposing (divideBy)
import Tuple.Extra exposing (pairWith, apply) -- See also pairWith for updates
import Maybe.Extra exposing (isNothing, unwrap)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Bootstrap.Dropdown as Dropdown
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Modal as Modal
import Bootstrap.Table as Table
import VegaLite exposing (Spec)


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
    -- , tail : Tail
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
    | ChangeLevel Float


-- update
applyBinomGen : List Float -> Model -> (Float -> Int) -> Model
applyBinomGen ws model gen =
    { model | ys = updateCountDict gen model.ys ws
            , trials = model.trials + List.length ws
    }

updateYs : List Float -> Model -> Model
updateYs ws model =
    model.binomGen
    |> unwrap model (applyBinomGen ws model)


updateLast : List Float -> Model -> Model
updateLast ws model =
  let
    lastX = Maybe.map2 Utility.apply model.binomGen (last ws)
    newBootstrap = Maybe.map2 updateSample lastX model.bootstrapSample
  in
    { model | bootstrapSample = newBootstrap }

updateOriginal : Maybe Sample -> Model -> Model
updateOriginal newSample model =
    { model |  originalSample = newSample }


resetBootstrap : Model -> Model
resetBootstrap model =
    { model |  bootstrapSample = model.originalSample |> Maybe.map makeEmptySample }


updateBinom : Maybe Sample -> Model -> Model
updateBinom newSample model =
    { model | binomGen = newSample |> updateBinomGen }


resetTrials : Model -> Model
resetTrials model =
    { model | trials = 0
            , ys = Dict.empty
            , level = Nothing
            , tailLimit = NoBounds
            }

updateSampleConfig : Model -> Sample -> DistPlotConfig
updateSampleConfig model s =
    s 
    |> sampleConfig model.trials model.ys model.tailLimit 

updateDistPlotConfig : Model -> Model
updateDistPlotConfig model =
   { model | distPlotConfig = model.originalSample |> Maybe.map (updateSampleConfig model) }



getBound : Model -> TailBound
getBound model =
    case (model.originalSample, model.level) of
        (Just sample, Just level) -> 
            getTwoTailBound model.trials level model.ys sample

        _ ->
            NoBounds

updateTailBounds : Model -> Model
updateTailBounds model =
    { model | tailLimit = getBound model}


andBatchCmds : List (Model -> Cmd.Cmd msg) -> Model -> (Model, Cmd msg)
andBatchCmds batch finalModel =
            ( finalModel
            , batch
              |> List.map (\b -> b finalModel)
              |> Cmd.batch 
            )

andRedrawPlots : Model -> (Model, Cmd Msg)
andRedrawPlots = andBatchCmds [ originalPlotCmd , bootstrapPlotCmd , distPlotCmd ] 



changeData : String -> Model -> Model
changeData name model =
    let
        data = model.datasets
                |> getNewData name
    in
        { model | selected = data , originalSample = Nothing} 


loadCsv : String -> Model -> Model
loadCsv content model =
      { model | csv = Just content , variables = getVariables content}


updateDataDropState : Dropdown.State -> Model -> Model
updateDataDropState state model =
    { model | dataDropState = state }


updateSuccessDropState : Dropdown.State -> Model -> Model
updateSuccessDropState state model =
    { model | successDropState = state }


updateFileName : File -> Model -> Model
updateFileName file model =
      { model | fileName = name file }


resetFile : Model -> Model
resetFile model =
    { model | fileName = "None"
            , fileType = NoType
            , variables = Nothing
            , selectedData = Nothing
            , selectedVariable = Nothing
            , counts = Nothing
    }


changeSuccess : String -> Model -> Model
changeSuccess name model =
    let
        newSample = model.selected
                    |>  Maybe.andThen (getSuccess name)
    in
        model
        |> updateOriginal newSample
        |> resetBootstrap 
        |> updateBinom newSample
        |> updateDistPlotConfig
        |> updateTailBounds


closeModal : Model -> Model
closeModal model =
    { model | modalVisibility = Modal.hidden }


showModal : Model -> Model
showModal model =
    { model | modalVisibility = Modal.shown }



loadData : DataSet -> Model -> Model
loadData data model =
    { model | selected = Just data 
    , originalSample = Nothing
    , datasets = data :: model.datasets
    , modalVisibility = Modal.hidden 
    } 

changeFileType : String -> Model -> Model
changeFileType typeString model =
    { model | fileType = typeString |> getFileType }


changeLevel : Float -> Model -> Model
changeLevel level model =
    { model | level = Just level }


selectVariable : String -> Model -> Model
selectVariable lbl model =
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
        { model | selectedData = col, selectedVariable = var}




selectCounts : String -> Model -> Model
selectCounts lbl model =
        let
            counts = 
                model.variables
                |> Maybe.map (findVariable lbl)
                |> Maybe.withDefault Nothing
                |> Maybe.map (List.map (String.toInt >> Maybe.withDefault 0))
        in
            { model | counts = counts }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CsvRequested ->
        model
        |> resetFile
        |> pairWith (Select.file ["text/csv"] CsvSelected)


    CsvSelected file ->
        model
        |> updateFileName file
        |> pairWith (Task.perform CsvLoaded (File.toString file))


    CsvLoaded content ->
        model
        |> loadCsv content
        |> resetTrials
        |> resetBootstrap 
        |> andRedrawPlots


    DataDropMsg state ->
        model
        |> updateDataDropState state
        |> pairWith Cmd.none


    SuccessDropMsg state ->
        model
        |> updateSuccessDropState state
        |> pairWith Cmd.none


    ChangeData name ->
        model
        |> changeData name
        |> resetTrials
        |> andRedrawPlots


    ChangeSuccess name ->
        model
        |> changeSuccess name
        |> andRedrawPlots 


    CloseModal ->
        model
        |> closeModal
        |> andRedrawPlots


    ShowModal ->
        model
        |> showModal
        |> pairWith Cmd.none


    ChangeFileType s ->
        model
        |> changeFileType s
        |> resetTrials
        |> pairWith Cmd.none


    SelectVariable lbl ->
        model
        |> selectVariable lbl
        |> updatePerspective
        |> resetTrials
        |> pairWith Cmd.none
        

    SelectCount lbl ->
        model
        |> selectCounts lbl
        |> updatePerspective
        |> resetTrials
        |> pairWith Cmd.none


    LoadUserData ->
        case model.perspectiveData of
            Nothing ->
                model
                |> resetTrials
                |> resetBootstrap
                |> andRedrawPlots
        
            Just data ->
                model
                |> loadData data
                |> resetTrials
                |> resetBootstrap
                |> andRedrawPlots


    Collect n ->
        model
        |> pairWith (Random.generate NewStatistics (Random.list n (Random.float 0 1)))

    
    Reset ->
        model
        |> resetBootstrap 
        |> resetTrials
        |> updateDistPlotConfig
        |> andRedrawPlots


    NewStatistics ws ->
        model 
        |> updateYs ws
        |> updateLast ws
        |> updateDistPlotConfig
        |> updateTailBounds
        |> andRedrawPlots

        
    ChangeLevel level ->
        model
        |> changeLevel level
        |> updateTailBounds
        |> updateDistPlotConfig
        |> andRedrawPlots



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
    model.selected
    |> isNothing

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
    model
    |> .variables
    |> Maybe.withDefault []
    |> List.map Tuple.first 
    |> List.map variableItem 
    |> (++) [ Select.item [ value "none"] [ text "Not Selected"]]
    

makeTableRow : LabelFreq -> Table.Row msg
makeTableRow labelFreq =
    Table.tr []
        [ Table.td [] [ labelFreq |> .label |> text  ]
        , Table.td [] [ labelFreq |> .count |> String.fromInt |> text  ]
        ]


dataDropdownConfig : Model -> DropdownConfig Msg
dataDropdownConfig model =
    { placeholder = dataPlaceholder model
    , label = "Data"
    , state = model.dataDropState
    , toggle = DataDropMsg
    , button = dataButtonToggle model
    , items = dropdownData model
    }


successDropdownConfig : Model -> DropdownConfig Msg
successDropdownConfig model =
    { placeholder = successPlaceholder model
    , label = "Success"
    , state = model.successDropState
    , toggle = SuccessDropMsg
    , button = successButtonToggle model
    , items = dropdownSuccess model
    }


dataDropdown : Model -> Html Msg
dataDropdown = dataDropdownConfig >> genericDropdown
    

successDropdown : Model -> Html Msg
successDropdown = successDropdownConfig >> genericDropdown 


noPerspectiveData : Model -> Bool
noPerspectiveData model =
    model.perspectiveData
    |> isNothing

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


dataTable : DataSet -> Html msg
dataTable data =
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


previewTable : Model -> Html Msg
previewTable model = 
    model.perspectiveData 
    |> unwrap (div [] []) dataTable


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



sampleGrid :  String -> String -> Html msg -> Html msg
sampleGrid label plotId s =
    div []
        [ Form.group []
            [ h4 [] [ Html.text label]
            , Grid.row []
                [ Grid.col [ Col.xs7 ] [ div [id plotId ] [] ]
                , Grid.col [ Col.xs5 ] [ s ]
                ]
            ]
        ]

originalSampleView : Model -> Html Msg
originalSampleView model =
    model
    |> .originalSample
    |> maybeSampleView
    |> sampleGrid "Original Sample" "samplePlot"


bootstrapSampleView : Model -> Html Msg
bootstrapSampleView model = 
    model
    |> .bootstrapSample
    |> maybeSampleView
    |> sampleGrid "Bootstrap Sample" "bootstrapPlot"

-- layout function
collectButtonGrid : Html msg -> Html msg -> Html msg -> Html msg
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



distPlotView : Model -> Html Msg
distPlotView model =
    if model.originalSample == Nothing  then
            div [] []
    else
            div [id "distPlot" ] []



outputText : Model -> String
outputText = .tailLimit >> confIntOutput


formRow : Col.Option msg -> Col.Option msg -> String -> Html msg -> List (Grid.Column msg)
formRow left right lbl content =
    [ Grid.col [ left ] [ b [] [text lbl ]]
    , Grid.col [ right ] [  content ]
    , Grid.colBreak []
    ]

formRows : Col.Option msg -> Col.Option msg -> List (String, Html msg) -> List (Grid.Column msg)
formRows left right comps = 
    comps
    |> List.map (Tuple.Extra.apply (formRow left right))
    |> List.foldr (++) []



formGroup : Col.Option msg -> Col.Option msg -> String -> List (String, Html msg)  -> Html msg
formGroup left right title components =
        div [] 
            [ Form.group []
                [ Grid.row []
                    [ Grid.col  [ Col.xs9 ]
                                [ Html.h4 [] [Html.text title]
                                , div []
                                        [ Grid.container []
                                                [ Grid.row []
                                                    (formRows left right components)
                                                ]
                                        ]
                                
                                ]
                        ]
                ]
            ]

confLimitsGrid : Html msg -> Html msg -> Html msg 
confLimitsGrid levelButtons output =
    let
        components =    [ ("Level",  levelButtons)
                        , ("Result", output)
                        ]
    in
        formGroup Col.sm3 Col.sm9 "Confidence Interval" components

collectGrid : Html msg -> Html msg -> Html msg 
collectGrid buttons resetButton =
    let
        components =    [ ("", buttons)
                        , ("", resetButton)
                        ]
    in
        formGroup Col.sm1 Col.sm11 "Collect Statistics" components

maybeShowControls : (Model -> Html msg) -> Model -> Html msg
maybeShowControls v model =
    if model.originalSample |> isNothing then
        div [] []
    else
        model |> v

confLimitsView : Model -> Html Msg
confLimitsView model =
    confLimitsGrid 
        (levelButtonView model)
        (model |> outputText |> text )


maybeConfIntView : Model -> Html Msg
maybeConfIntView = maybeShowControls confLimitsView

collectView : Model -> Html Msg
collectView _ =
    collectGrid 
        (collectButtons Collect defaults.collectNs)
        ( resetButton Reset)

maybeCollectView : Model -> Html Msg
maybeCollectView = maybeShowControls collectView


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


distSummaryGrid : Html msg -> Html msg -> Html msg -> Html msg 
distSummaryGrid mean sd nSamples =
    let
        components =    [ ("Mean",   mean)
                        , ("SE",  sd)
                        , ("Samples", nSamples)
                        ]
    in
        formGroup Col.sm4 Col.sm8 "Bootstrap Distribution" components



distSummaryView : Model -> Html Msg
distSummaryView model =
    let
        meanStr = model.originalSample
            |> Maybe.map (meanProp model.trials model.ys >> roundFloat 4)
            |> unwrap "??" String.fromFloat
            |> text
        -- adjust so that we divide by N - 1
        n = model.originalSample
            |> unwrap 1000 .n
        sdStr = model.originalSample   
                |> Maybe.map (sdProp model.trials model.ys >> roundFloat 4)
                |> unwrap "??" String.fromFloat
                |> text

        nSamples = model.trials
                 |> String.fromInt
                 |> text
 
    in
        distSummaryGrid meanStr sdStr nSamples 

maybeDistSummaryView : Model -> Html Msg
maybeDistSummaryView = maybeShowControls distSummaryView

mainGrid : Html msg -> Html msg -> Html msg -> Html msg -> Html msg -> Html msg -> Html msg -> Html msg-> Html msg
mainGrid dataEntry originalSample bootstrapSample collectButtons distPlot confLimits distSummary debug =
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
                                    , distSummary
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
            (maybeCollectView model)
            (distPlotView model)
            (maybeConfIntView model)
            (maybeDistSummaryView model)
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