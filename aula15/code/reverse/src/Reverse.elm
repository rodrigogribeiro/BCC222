module Reverse exposing (..)

import Browser         exposing (..)
import Html            exposing (..)
import Html.Attributes exposing (..)
import Tuple           exposing (first, second)

-- model

type alias Model = String

initialModel : Model
initialModel = "Hello World!"

-- CSS

style1 : (String, String) -> Attribute msg
style1 p = style (first p) (second p)

myStyle = List.map style1
            [ ("width", "100%")
            , ("height", "40px")
            , ("padding", "10px 0")
            , ("font-size", "2em")
            , ("text-align", "center") ]

-- view

view : Model -> Html msg
view m = div [ id "content"]
             [ h1 myStyle
                  [ text "My first Elm app" ]
                  , p myStyle
                      [ text (String.reverse m) ]
             ]

main = view initialModel
