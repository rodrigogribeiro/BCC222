module ReverseText exposing (..)

import Browser         exposing (..)
import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import Tuple           exposing (first, second)

-- model

type alias Model = String

-- CSS

style1 : (String, String) -> Attribute msg
style1 p = style (first p) (second p)

myStyle = List.map style1
            [ ("width", "100%")
            , ("height", "40px")
            , ("padding", "10px 0")
            , ("font-size", "2em")
            , ("text-align", "center") ]

-- controler

type alias Msg = String

update : Msg -> Model -> Model
update msg m = String.reverse msg

-- view

view : Model -> Html Msg
view m = div [ id "content"]
             [ h1 myStyle
                  [ text "My first Elm app" ]
             , input ([ placeholder "Digite aqui"
                     , onInput identity ] ++ myStyle)
                     []
             , p myStyle
                 [ text m ]
             ]

-- main function

main =
  Browser.sandbox { init = "", update = update, view = view }
