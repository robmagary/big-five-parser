module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, h1, label, span, text, textarea)
import Html.Attributes exposing (class, for, id, rows, value)
import Html.Events exposing (onClick, onInput)
import Input
import Parser exposing (DeadEnd)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type Msg
    = DataInput String
    | ParseRawData


type alias Model =
    { domain : Maybe (List Input.Domain)
    , parserFeedback : List DeadEnd
    , rawData : String
    , uiState : UiState
    }


init : Model
init =
    { domain = Nothing
    , parserFeedback = []
    , rawData = ""
    , uiState = InputtingRawData
    }


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
update msg model =
    case msg of
        DataInput input ->
            { model | rawData = input }

        ParseRawData ->
            let
                ( maybeParsedDomain, maybeFeedback ) =
                    case Input.parse model.rawData of
                        Ok parsedDomain ->
                            ( Just parsedDomain
                            , []
                            )

                        Err listOfDeadends ->
                            ( Nothing
                            , listOfDeadends
                            )
            in
            { model
                | uiState = ParsingRawData
                , domain = maybeParsedDomain
                , parserFeedback = maybeFeedback
            }


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
                , span [ class "badge bg-primary" ] [ text <| uiStateToString model.uiState ]
                ]
            , label [ for textAresId, class "form-label" ] [ text "Parser Input" ]
            , textarea
                [ id textAresId
                , class "form-control mb-3"
                , rows 10
                , value model.rawData
                , onInput DataInput
                ]
                []
            , button
                [ class "btn btn-primary"
                , onClick ParseRawData
                ]
                [ text "Parse" ]
            ]
        ]
