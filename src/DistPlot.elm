module DistPlot exposing (..)

import CountDict exposing (..)
import Dict exposing (..)
import DataEntry exposing (..)
import Limits exposing (..)
import Defaults exposing (defaults)
import VegaLite exposing (..)
import Double exposing (..)



combineDistColumns : Count -> (List Int, List Int) -> (List Int , List Int)
combineDistColumns pair columns =
  let
    (newX, newY) = pair
    (oldXs, oldYs) = columns
  in
    (newX :: oldXs, newY :: oldYs)


distColumns : CountDict -> (List Float, List Float)
distColumns yDict =
  yDict
  |> Dict.toList
  |> List.foldl combineDistColumns ([], [])
  |> Tuple.mapBoth (List.map toFloat) (List.map toFloat)


countPairToDots : Count -> (List Int, List Int)
countPairToDots pair =
  let
    (x, cnt) = pair
    xs = List.repeat cnt x
    ys = List.range 1 cnt
  in
    (xs, ys)


combineDotColumns : Count -> (List Int, List Int) -> (List Int, List Int)
combineDotColumns nextPair columns =
  let
    (newXs, newYs) = countPairToDots nextPair
    (oldXs, oldYs) = columns
  in
    (newXs ++ oldXs, newYs ++ oldYs)


dotColumns : CountDict -> (List Float, List Float)
dotColumns yDict =
  let
    countList = Dict.toList yDict
  in
    countList
    |> List.foldl combineDotColumns ([], [])
    |> Tuple.mapBoth (List.map toFloat) (List.map toFloat)


countPairToHeights : Count ->  List Int
countPairToHeights pair =
  let
    (_, cnt) = pair
  in
    List.repeat cnt cnt


combineHeights : Count -> List Int -> List Int
combineHeights nextPair heights =
    (countPairToHeights nextPair) ++ heights

dotColumnHeights : CountDict -> List Float
dotColumnHeights yDict = 
    yDict
    |> Dict.toList 
    |> List.foldl combineHeights [] 
    |> List.map toFloat




shiftLimits : Int -> Limits -> Limits
shiftLimits n pair =
    pair
    |> mapBoth (\i -> i - 2)  (\i -> i + 2)
    |> mapBoth (Basics.max 0) (Basics.min n)



type alias DistPlotConfig = { tailExpression : String
                            , xs : List Float
                            , ys : List Float
                            , maxY : Float
                            , minX : Float
                            , maxX : Float
                            , mark : List MarkProperty -> ( VLProperty, Spec )
                            , xAxisTitle : String
                            }


getTailExpression : Float -> TailBound -> String
getTailExpression mean tailLimit =
  case tailLimit of
    NoBounds ->
      "false"

    Lower l ->
      "datum.X <= " ++ (String.fromFloat l)

    Upper u ->
      "datum.X >= " ++ (String.fromFloat u)

    TwoTail l u ->
      if (mean == l) || (mean == u) then 
        "true"
      else 
        [ "datum.X <="
        , l |> String.fromFloat 
        , "|| datum.X >=" 
        , u |> String.fromFloat
        ] |> String.join " "

getXandY : Bool -> CountDict -> (List Float, List Float)
getXandY isLarge countDict =
    if isLarge then
        distColumns countDict
    else
        dotColumns countDict

maybeMakeProp : Bool  -> Float -> List Float -> List Float
maybeMakeProp isProportion n xs =
            if isProportion then
                xs |> List.map (\x -> x/n)
            else
                xs

getMaxHeight : CountDict -> Float
getMaxHeight countDict = 
    Basics.max defaults.distMinHeight (countDict |> tallestBar |> toFloat)


getXAxisTitle : Bool -> String -> String
getXAxisTitle isProp successLbl =
    if isProp then
        "Proportion of " ++ successLbl
    else
        "Count of " ++ successLbl


-- NOTE 1: plotLimits are now passed as a function to the plot
--       and should be an application specific defined in Main 
--       with a type of DistPlotConfig -> (Float, Float)
distPlot :  DistPlotConfig -> Spec
distPlot config =
    let
        d = dataFromColumns []
            << dataColumn "X" (nums config.xs)
            << dataColumn "N(X)" (nums config.ys)

        trans =
            transform
                << VegaLite.filter (fiExpr config.tailExpression)

        encPMF =
            encoding
                << position X [ pName "X"
                              , pMType Quantitative
                              , pScale [scDomain (doNums [config.minX, config.maxX])]
                              ]
                << position Y [ pName "N(X)"
                              , pAggregate opSum
                              , pMType Quantitative
                              , pScale [scDomain (doNums [0.0, config.maxY])]
                              ]
                << tooltips [ [ tName "X", tMType Quantitative]
                            , [ tName "N(X)", tMType Quantitative, tFormat ".0f"]
                            ]

        selectedEnc =
            encoding
                << position X [ pName "X"
                              , pMType Quantitative
                              , pAxis [axTitle config.xAxisTitle]
                              ]
                << position Y [ pName "N(X)"
                              ,  pMType Quantitative 
                              , pAxis [axTitle "Frequency"]
                              ]
                << tooltips [ [ tName "X", tMType Quantitative]
                            , [ tName "N(X)",  tFormat ".0f"]
                            ]
                << color [ mStr "red", mLegend []]


    in
        toVegaLite
            [ VegaLite.width defaults.distPlotWidth
            , VegaLite.height defaults.distPlotHeight
            ,
            d []
            , layer [ asSpec [ config.mark [], encPMF []]
                    , asSpec  [ config.mark [], selectedEnc [], trans []]
                    ]
            ]