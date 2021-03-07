module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, form, h1, input, label, li, ol, span, text, textarea, ul)
import Html.Attributes exposing (class, for, id, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Parser exposing (DeadEnd)
import Type.Description as Description exposing (Description)
import Type.Domain as Domain
import Type.Facet as Facet exposing (Facet)
import Type.Submission as Submission exposing (Submission)


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
    | ResetUi
    | CreateSubmision


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
    | CreatingSubmission Submission


uiStateToString : UiState -> String
uiStateToString state =
    case state of
        InputtingRawData ->
            "Inputting Data"

        DataParseError _ ->
            "Data Parse Error"

        ReviewingResult _ ->
            "Reviewing Parse Result"

        CreatingSubmission _ ->
            "Inputting Contact Info"


update : Msg -> Model -> Model
update msg model =
    case msg of
        ResetUi ->
            init

        CreateSubmision ->
            case model.uiState of
                ReviewingResult descriptions ->
                    { model
                        | uiState = CreatingSubmission (Submission.init descriptions)
                    }

                _ ->
                    model

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

        DataParseError _ ->
            [ text "Oh no, deadend" ]

        ReviewingResult descriptionList ->
            renderParseResult descriptionList

        CreatingSubmission submission ->
            renderSubmissionForm submission


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


nextStepOrRestart : Html Msg -> Html Msg
nextStepOrRestart nextStep =
    div [ class "d-flex flex-row justify-content-between mt-3 mb-5" ]
        [ button [ class "btn btn btn-secondary", onClick ResetUi ] [ text "Start Over" ]
        , nextStep
        ]


renderParseResult : List Description -> List (Html Msg)
renderParseResult descriptionList =
    [ ol [ class "mt-3" ] <|
        List.map
            (\description ->
                li []
                    [ text <| Domain.toString description.domain
                    , renderfacetList description.facets
                    ]
            )
            descriptionList
    , nextStepOrRestart <| button [ class "btn btn-primary", onClick CreateSubmision ] [ text "Looks good" ]
    ]


renderfacetList : List Facet -> Html msg
renderfacetList facets =
    ul [ class "mb-3" ] <|
        List.map
            (\facet ->
                li [] [ text <| Facet.toString facet ]
            )
            facets


renderSubmissionForm : Submission -> List (Html Msg)
renderSubmissionForm submission =
    let
        nameId =
            "nameInput"

        emailId =
            "emailInput"
    in
    [ form []
        [ label [ class "form-label mt-3", for nameId ] [ text "First and Last Name" ]
        , input
            [ class "form-control"
            , id nameId
            , type_ "text"
            , value submission.name
            ]
            []
        , label [ class "form-label mt-3", for emailId ] [ text "Email Address" ]
        , input
            [ class "form-control"
            , id emailId
            , type_ "email"
            , value submission.email
            ]
            []
        , nextStepOrRestart <| input [ class "btn btn-primary mt-3", type_ "submit" ] [ text "Send it" ]
        ]
    ]
