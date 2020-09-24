module Hello exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


-- model

type alias Model = String

initialModel : Model
initialModel = "Hello World!"

-- view

view : Model -> Html msg 
view m = div [ id "content" ]
             [ h1 [] [ text "Hello World!!" ]
             , p  [] [ text m ]
             ]

main : Html msg
main = view initialModel
