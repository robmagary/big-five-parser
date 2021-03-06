module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, h1, label, span, text, textarea)
import Html.Attributes exposing (class, for, id, rows)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox { init = InputtingRawData, update = update, view = view }


type Msg
    = ParseRawData


type alias Model =
    UiState


type UiState
    = InputtingRawData
    | ParsingRawData


uiStateToString : UiState -> String
uiStateToString state =
    case state of
        InputtingRawData ->
            "Inputting Data"

        ParsingRawData ->
            "Parsing..."


update : Msg -> Model -> Model
update msg _ =
    case msg of
        ParseRawData ->
            ParsingRawData


view : Model -> Html Msg
view model =
    let
        textAresId =
            "parserTextarea"
    in
    div [ class "container pt-3" ]
        [ div []
            [ div [ class "d-flex  justify-content-between align-items-center" ]
                [ h1 [] [ text "Big 5 Parser" ]
                , span [ class "badge bg-primary" ] [ text <| uiStateToString model ]
                ]
            , label [ for textAresId, class "form-label" ] [ text "Parser Input" ]
            , textarea [ id textAresId, class "form-control mb-3", rows 10 ] []
            , button
                [ class "btn btn-primary"
                , onClick ParseRawData
                ]
                [ text "Parse" ]
            ]
        ]
