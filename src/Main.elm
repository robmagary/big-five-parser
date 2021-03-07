module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, h1, label, li, ol, span, text, textarea)
import Html.Attributes exposing (class, for, id, rows, value)
import Html.Events exposing (onClick, onInput)
import Parser exposing (DeadEnd)
import Type.Description as Description exposing (Description)
import Type.Domain as Domain


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
    { rawData : String
    , uiState : UiState
    }


init : Model
init =
    { rawData = ""
    , uiState = InputtingRawData
    }


type UiState
    = InputtingRawData
    | DataParseError (List DeadEnd)
    | ReviewingResult (List Description)


uiStateToString : UiState -> String
uiStateToString state =
    case state of
        InputtingRawData ->
            "Inputting Data"

        DataParseError _ ->
            "Data Parse Error"

        ReviewingResult _ ->
            "Reviewing Parse Result"


update : Msg -> Model -> Model
update msg model =
    case msg of
        DataInput input ->
            { model | rawData = input }

        ParseRawData ->
            let
                maybeOrderedDomains =
                    Domain.orderList Domain.list model.rawData

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

                        newUiState =
                            case Description.parseList descriptionSectionList of
                                Ok parsedDescriptions ->
                                    ReviewingResult parsedDescriptions

                                Err listOfDeadends ->
                                    DataParseError listOfDeadends
                    in
                    { model
                        | uiState = newUiState
                    }

                _ ->
                    { model
                        | uiState = DataParseError []
                    }


view : Model -> Html Msg
view model =
    div [ class "container pt-3" ]
        [ div [] <|
            div [ class "d-flex  justify-content-between align-items-center" ]
                [ h1 [] [ text "Big 5 Parser" ]
                , span [ class "badge bg-primary" ] [ text <| uiStateToString model.uiState ]
                ]
                :: renderUiByState model
        ]


renderUiByState : Model -> List (Html Msg)
renderUiByState model =
    let
        rawData =
            model.rawData

        uiState =
            model.uiState
    in
    case uiState of
        InputtingRawData ->
            renderInputtingUi rawData

        DataParseError deadEndList ->
            [ text "Oh no, deadend" ]

        ReviewingResult descriptionList ->
            renderParseResult descriptionList


renderInputtingUi : String -> List (Html Msg)
renderInputtingUi rawData =
    let
        textAreaId =
            "parserTextarea"
    in
    [ label [ for textAreaId, class "form-label" ] [ text "Parser Input" ]
    , textarea
        [ id textAreaId
        , class "form-control mb-3"
        , rows 10
        , value rawData
        , onInput DataInput
        ]
        []
    , button
        [ class "btn btn-primary"
        , onClick ParseRawData
        ]
        [ text "Parse" ]
    ]


renderParseResult : List Description -> List (Html msg)
renderParseResult descriptionList =
    [ ol [] <|
        List.map
            (\description ->
                li [] [ text <| Domain.toString description.domain ]
            )
            descriptionList
    ]
