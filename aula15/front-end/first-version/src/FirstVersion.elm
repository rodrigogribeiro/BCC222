module FirstVersion exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import String 
import Json.Decode as D 


-- main function

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- defining products

type alias Product
    = {
        code        : Int
      , name        : String
      , description : String
      , price       : Float
      , quantity    : Int
      }

-- model

type Model
    = Failure Http.Error
    | Loading
    | Success (List Product)

-- messages

type Msg
    = Waiting
    | GotIt (Result Http.Error
                    (List Product))

-- init

init : () -> (Model, Cmd Msg)
init _ = (Loading, getProducts)

-- update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model
    = case msg of
        Waiting   -> (Loading, getProducts)
        GotIt res ->
            case res of
              Ok ps -> (Success ps, Cmd.none)
              Err err -> (Failure err, Cmd.none)

-- view

productRow : Product -> Html Msg
productRow p
    = tr [] [
        td [] [text (String.fromInt p.code)]
      , td [] [text p.name]
      , td [] [text p.description]
      , td [] [text (String.fromFloat p.price)]
      , td [] [text (String.fromInt p.quantity)]
      ]

viewError : Http.Error -> Html Msg
viewError err
    = case err of
        Http.BadUrl str   ->
            div [][
               text (String.append "Bad url error " str)
            ]
        Http.Timeout      ->
            div [] [
               text "Timeout error"
            ]
        Http.NetworkError ->
            div [] [
               text "Network error"
            ]
        Http.BadStatus n  ->
            div [] [
               text (String.append "Bad Status:" (String.fromInt n))
            ]
        Http.BadBody str  ->
            div [] [
               text (String.append "Bad Body:" str)
            ]

view : Model -> Html Msg
view model
    = case model of
         Failure err ->
             viewError err
         Loading ->
           div [class "container-fluid"] [
                 text "Loading..."
               ]
         Success ps ->
           div [class "container-fluid"] [
                h1 [] [text "Products"]
               , table [class "table table-stripped"] [
                   thead [] [
                     tr [] [
                       th [] [text "Code"]
                     , th [] [text "Name"]
                     , th [] [text "Description"]
                     , th [] [text "Price"]
                     , th [] [text "Quantity"]
                     ]
                   ]
                   , tbody [] (List.map productRow ps)
                ]
              ]

-- get products from server

getProducts : Cmd Msg
getProducts
    = Http.get {
        url = "http://localhost:8081/products"
      , expect = Http.expectJson GotIt decodeProducts
      }

-- decode product JSON

decodeProducts : D.Decoder (List Product)
decodeProducts
    = D.list decodeProduct


decodeProduct : D.Decoder Product
decodeProduct
    = D.map5 Product (D.field "code" D.int)
                     (D.field "name" D.string)
                     (D.field "description" D.string)
                     (D.field "price" D.float)
                     (D.field "quantity" D.int)
