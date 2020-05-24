module Limits exposing (..)

import Double exposing (..)
import Display exposing (stringAndAddCommas)
import Binomial exposing (roundFloat)
import CountDict exposing (Count, CountDict)
import Dict
import Defaults exposing (defaults)

type alias Limits = (Int, Int)

combineTwoLimits : Limits -> Limits -> Limits
combineTwoLimits lim1 lim2 =
  let
    (min1, max1) = lim1
    (min2, max2) = lim2
  in
    ( Basics.min min1 min2
    , Basics.max max1 max2
    )


updateLimits : Maybe Limits -> Maybe Limits -> Maybe Limits
updateLimits next current =
  case (next, current) of
    (Nothing, Nothing) ->
      Nothing

    ( _ , Nothing) ->
      next

    (Nothing, _ ) ->
      current

    (Just p1, Just p2) ->
      combineTwoLimits p1 p2
      |> Just

      
combineLimits : List (Maybe Limits) -> Maybe Limits
combineLimits = List.foldl updateLimits Nothing


updatePairLimits : Count -> Maybe Limits -> Maybe Limits
updatePairLimits nextPair currentMinMax =
  let
    (x, _ ) = nextPair
    xLim = (x, x)
  in
    case currentMinMax of
      Nothing ->
        Just xLim

      Just currentLim ->
        combineTwoLimits xLim currentLim
        |> Just


pairLimits : Int -> List Count -> Maybe Limits
pairLimits n = List.foldl updatePairLimits Nothing


countLimits : Int -> CountDict -> Maybe Limits
countLimits n = pairLimits n << Dict.toList

type Tail = Left | Right | Two | None

type TailValue a = NoBounds | Lower a | Upper a | TwoTail a a 

type alias TailBound = TailValue Float
type alias TailCount = TailValue Int


lowerCount : Float -> Count -> Int
lowerCount l pair =
  let 
    (cnt, freq) = pair
    cntF = cnt |> toFloat
  in
    if cntF <= l then freq else 0


upperCount : Float -> Count -> Int
upperCount u pair =
  let 
    (cnt, freq) = pair
    cntF = cnt |> toFloat
  in
    if cntF >= u then freq else 0

inTailCount : TailBound -> Count -> TailCount
inTailCount tailLim pair =
  case tailLim of
    NoBounds ->
      NoBounds
      
    Lower l ->
      pair |> lowerCount l |> Lower
    
    Upper u ->
      pair |> upperCount u |> Upper

    TwoTail l u ->
      TwoTail
        (pair |> lowerCount l)
        (pair |> upperCount u)


startingTailCount : TailBound -> TailCount
startingTailCount tailBound =
    case tailBound of
        NoBounds ->
            NoBounds

        Lower _ ->
            Lower 0

        Upper _ ->
            Upper 0

        TwoTail _ _ ->
            TwoTail 0 0


addTails : TailCount -> TailCount -> TailCount
addTails pval1 pval2 =
  case (pval1, pval2) of
    (Lower f1, Lower f2) ->
      Lower (f1 + f2)

    (Upper f1, Upper f2) ->
      Upper (f1 + f2)

    (TwoTail l1 u1, TwoTail l2 u2) ->
      TwoTail (l1 + l2) (u1 + u2)

    _ ->
      NoBounds


totalTailCount : TailBound -> CountDict -> TailCount
totalTailCount bound counts =
    counts
    |> Dict.toList
    |> List.map (inTailCount bound)
    |> List.foldl addTails (startingTailCount bound)


finalTailCount : TailCount -> Maybe (Int, Int)
finalTailCount totalCount =
    case totalCount of
        NoBounds ->
            Nothing

        Lower l ->
            Just (l, 0)

        Upper u ->
            Just (0, u)

        TwoTail l u ->
            Just (l, u)

finalTailProp : Int -> TailCount -> Maybe Float
finalTailProp trials totalCount =
    let
        tailCounts = finalTailCount totalCount
        divByTrials = \i -> (toFloat i)/(toFloat trials)
    in
        tailCounts
        |> Maybe.map (Tuple.mapBoth divByTrials divByTrials >> \tup -> Tuple.first tup + Tuple.second tup)


numerator : TailCount -> String
numerator tailTotal =
  case tailTotal of
    NoBounds ->
       "??"
    
    Lower l ->
      l |> stringAndAddCommas
  
    Upper u ->
      u |> stringAndAddCommas

    TwoTail l u ->
      [ "(" ++  (l |> stringAndAddCommas)
      , "+"
      , (u |> stringAndAddCommas) ++ ")"
      ] |> String.join " "

proportion : Int -> TailCount -> String
proportion trials tailTotal =
  let
    prop = finalTailProp trials tailTotal
  in
    case (tailTotal, prop) of
      (NoBounds, _) ->
        "??"
      
      (_, Nothing) ->
        "??"

      (_, Just p) ->
        p |> roundFloat defaults.pValDigits |> String.fromFloat
    
     