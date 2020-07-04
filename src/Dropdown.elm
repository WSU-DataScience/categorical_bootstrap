module Dropdown exposing (..)


import Html exposing (Html, text, b)
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Dropdown as Dropdown

type alias DropdownConfig msg = 
    { placeholder : String
    , label : String
    , toggle : Dropdown.State -> msg
    , state : Dropdown.State
    , button : Dropdown.DropdownToggle msg
    , items : List (Dropdown.DropdownItem msg)
    }


genericDropdown : DropdownConfig msg -> Html msg
genericDropdown config =
    InputGroup.config
        ( InputGroup.text [ Input.placeholder config.placeholder, Input.disabled True] )
        |> InputGroup.small
        |> InputGroup.predecessors
            [ InputGroup.span [] [ b [] [text config.label]] ]
        |> InputGroup.successors
                [ InputGroup.dropdown
                    config.state
                    { options = [ Dropdown.alignMenuRight ]
                    , toggleMsg = config.toggle
                    , toggleButton = config.button
                    , items = config.items
                    }
                ]
        |> InputGroup.view
