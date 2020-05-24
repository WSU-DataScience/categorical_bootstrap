module CollectButtons exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Display exposing (stringAndAddCommas, changeThousandsToK)

resetButton : msg -> Html msg
resetButton onClickMsg =
    Button.button
        [ Button.primary
        , Button.onClick onClickMsg
        , Button.small
        ]
        [ Html.text "Reset" ]


totalCollectedTxt : Int -> Html msg
totalCollectedTxt numTrials =
    (numTrials |> stringAndAddCommas) ++ " statistics collected" |> Html.text


collectButton : (Int -> msg) -> Int -> ButtonGroup.ButtonItem msg
collectButton toOnClick n =
    ButtonGroup.button 
      [ Button.primary
      , Button.onClick (toOnClick n)
      ] 
      [  Html.text (changeThousandsToK n) ]

collectButtons : (Int -> msg) -> List Int -> Html msg
collectButtons toOnClick ns =
    div []
        [ ButtonGroup.buttonGroup
            [ ButtonGroup.small, ButtonGroup.attrs [ style "display" "block"] ]
            (List.map (collectButton toOnClick) ns)
        ]