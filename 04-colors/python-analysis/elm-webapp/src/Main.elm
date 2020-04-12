module Main exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Task


-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { csv : Maybe String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Nothing, Cmd.none )



-- UPDATE


type Msg
  = TextFileRequested
  | TextFileSelected File
  | TextFileLoaded String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TextFileRequested ->
      ( model
      , Select.file ["text/plain"] TextFileSelected
      )

    TextFileSelected file ->
      ( model
      , Task.perform TextFileLoaded (File.toString file)
      )

    TextFileLoaded content ->
      ( { model | csv = Just content }
      , Cmd.none
      )



-- VIEW


view : Model -> Html Msg
view model =
  case model.csv of
    Nothing ->
      button [ onClick TextFileRequested ] [ text "Upload Text File" ]
    Just content ->
      p [ style "white-space" "pre" ] [ text content ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
