module Sample exposing (..)


import DataSet exposing (DataSet, getN)
import CountDict exposing (CountDict, getPercentiles )
import Limits exposing (TailBound, TailValue(..))
import DistPlot exposing (DistPlotConfig, getTailExpression, getXandY, getMaxHeight)
import Defaults exposing (defaults)
import List.Extra exposing (find, group, last, takeWhile, dropWhileRight)
import Binomial exposing (roundFloat, getBinomGen)
import Maybe exposing (withDefault)
import Maybe.Extra exposing (isNothing, unwrap)
import Template exposing (template, render, withValue, withString)
import Html exposing (Html, button, text, div, h3, h5, h4, b, br)
import Html.Attributes exposing (style, value, class, id)
import VegaLite exposing (circle, bar)

type alias Sample = { successLbl : String
                     , failureLbl : String
                     , numSuccess : Int
                     , numFailures : Int
                     , n : Int
                     , p : Maybe Float
                     }

makeEmptySample : Sample -> Sample
makeEmptySample sample =
    { sample | numSuccess = 0, numFailures = 0, p = Nothing}

divide : Int -> Int -> Maybe Float
divide x n =
    case n of
       0 ->
        Maybe.Nothing

       _ ->
        (toFloat x)/(toFloat n)
        |> Just

updateSample : Int -> Sample -> Sample
updateSample x sample =
    { sample | numSuccess = x, numFailures = sample.n - x, p = divide x sample.n }


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
                                    , p = divide labelFreq.count n
                                    })

isEmpty = .p >> isNothing

nStr : Sample -> String
nStr sample =
    template "N = "
    |> Template.withValue (.n >> String.fromInt)
    |> render sample


countStr : Sample -> String
countStr sample =
    template "Count("
    |> Template.withValue .successLbl
    |> withString ") = "
    |> Template.withValue (.numSuccess >> String.fromInt)
    |> render sample


propStr : Sample -> String
propStr sample =
    template "Prop("
    |> Template.withValue .successLbl
    |> withString ") = "
    |> withValue (.p >> withDefault 0 >> roundFloat 3 >> String.fromFloat)
    |> render sample

sampleView : Sample -> Html msg
sampleView sample =
    if sample |> isEmpty then   
        div [] [] 
    else
        div [ style "font-size" "smaller"] 
            [ sample |> nStr |> text
            , br [] []
            , sample |> countStr |> text
            , br [] []
            , sample |> propStr |> text
            , br [] []
            ]

maybeSampleView : Maybe Sample -> Html msg
maybeSampleView = unwrap (div [] []) sampleView

binomGen : Sample -> Maybe (Float -> Int)
binomGen sample = 
    Maybe.map (getBinomGen sample.n) sample.p


updateBinomGen : Maybe Sample -> Maybe (Float -> Int)
updateBinomGen sample =
    sample
    |> unwrap Nothing binomGen



sampleConfig : Int -> CountDict -> TailBound -> Sample -> DistPlotConfig
sampleConfig trials countDict tailLimit sample =
        let
            n = sample.n |> toFloat
            p = sample.numSuccess
                |> toFloat
                |> \x -> x/n
            mean = n*p
            tailExpr = getTailExpression mean tailLimit
            isLarge = trials > defaults.largePlot
            (xs, ys) = getXandY isLarge countDict
            smallestX = Maybe.withDefault 0 (List.minimum xs) - 3
            largestX = Maybe.withDefault n (List.maximum xs) + 3 
            ps = xs |> List.map (\x -> x/n)
        in
            { tailExpression = tailExpr
            , xs = ps
            , ys = ys
            , maxY = getMaxHeight countDict
            , minX = Basics.max 0 (smallestX/n)
            , maxX = Basics.min 1 (largestX/n)
            , mark = if isLarge then bar else circle
            , xAxisTitle = "Proportion of " ++ sample.successLbl
            }

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


oneTailBound : (Float -> List (Float, Float) -> Maybe Float) -> (Float -> TailValue Float) -> Int -> Float -> CountDict -> Sample -> TailValue Float
oneTailBound boundFunc tailFunc trials level countDict sample =
    let
        tailArea = 1 - level
    in
        countDict
        |> getPercentiles sample.n trials
        |> boundFunc tailArea
        |> Maybe.map tailFunc
        |> Maybe.withDefault NoBounds


getLeftTailBound : Int -> Float -> CountDict -> Sample -> TailValue Float
getLeftTailBound = oneTailBound leftTailBound Lower



getRightTailBound : Int -> Float -> CountDict -> Sample -> TailValue Float
getRightTailBound = oneTailBound rightTailBound Upper



getTwoTailBound : Int -> Float -> CountDict -> Sample -> TailValue Float
getTwoTailBound trials level countDict sample = 
            let
                tailArea = (1 - level)/2
                percentiles = countDict
                        |> getPercentiles sample.n trials
                left = percentiles
                        |> leftTailBound tailArea
                right = percentiles
                        |> rightTailBound tailArea
            in
                Maybe.map2 TwoTail left right
                |> Maybe.withDefault NoBounds