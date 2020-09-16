module Hello exposing (..)

import Browser

import Html exposing (..)

main
    = Browser.sandbox
      {
        init = init
      , update = \ _ model -> model
      , view = view }

type alias Model = { currentName : String }

init : Model
init = { currentName = "Programação Funcional" }

view : Model -> Html ()
view model
    = div []
          [ text "Bem vindo, "
          , text model.currentName
          , text "!"]
