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
    { descriptions : Maybe (List Input.Description)
    , parserFeedback : List DeadEnd
    , rawData : String
    , uiState : UiState
    }


init : Model
init =
    { descriptions = Nothing
    , parserFeedback = []
    , rawData = ""
    , uiState = InputtingRawData
    }


type UiState
    = InputtingRawData
    | InputDataError String
    | ReviewingResult


uiStateToString : UiState -> String
uiStateToString state =
    case state of
        InputtingRawData ->
            "Inputting Data"

        InputDataError error ->
            error

        ReviewingResult ->
            "Reviewing Parse Result"


update : Msg -> Model -> Model
update msg model =
    case msg of
        DataInput input ->
            { model | rawData = input }

        ParseRawData ->
            let
                maybeOrderedDomains =
                    Input.orderDomains Input.domains model.rawData

                sectionedRawData =
                    String.split "Domain/Facet...... Score" model.rawData
            in
            case ( maybeOrderedDomains, sectionedRawData ) of
                ( Just [ d1, d2, d3, d4, d5 ], [ _, section1, section2, section3, section4, section5 ] ) ->
                    let
                        descriptionSectionList =
                            [ ( d1, section1 )
                            , ( d2, section2 )
                            , ( d3, section3 )
                            , ( d4, section4 )
                            , ( d5, section5 )
                            ]

                        ( maybeParsedDescriptions, maybeFeedback ) =
                            case Input.parseDescriptions descriptionSectionList of
                                Ok parsedDescriptions ->
                                    ( Just parsedDescriptions
                                    , []
                                    )

                                Err listOfDeadends ->
                                    ( Nothing
                                    , listOfDeadends
                                    )
                    in
                    { model
                        | uiState = ReviewingResult
                        , descriptions = maybeParsedDescriptions
                        , parserFeedback = maybeFeedback
                    }

                _ ->
                    { model
                        | uiState = InputDataError "Input data has a problem with domains"
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
