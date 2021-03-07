module Type.Domain exposing (Domain(..), Label(..), Score(..), list, orderList, parser, toString)

import Parser exposing ((|.), (|=), Parser, Step(..), chompIf, chompUntil, chompWhile, getChompedString, int, succeed)


type Domain
    = Domain Label Score


type Label
    = Label String


type Score
    = Score Int


list : List Label
list =
    [ Label "EXTRAVERSION"
    , Label "AGREEABLENESS"
    , Label "CONSCIENTIOUSNESS"
    , Label "NEUROTICISM"
    , Label "OPENNESS TO EXPERIENCE"
    ]


orderList : List Label -> String -> Maybe (List Label)
orderList unOrdered rawData =
    let
        matchedOrdered =
            List.foldl
                (\line ordered ->
                    let
                        maybeMatchedLabel : Maybe Label
                        maybeMatchedLabel =
                            List.foldl
                                (\(Label label) maybeResult ->
                                    if String.startsWith label line then
                                        Just (Label label)

                                    else
                                        maybeResult
                                )
                                Nothing
                                unOrdered

                        anyMatch : Bool
                        anyMatch =
                            List.any
                                (\(Label label) ->
                                    String.startsWith label line
                                )
                                unOrdered
                    in
                    case ( anyMatch, maybeMatchedLabel ) of
                        ( True, Just domainLabel ) ->
                            domainLabel :: ordered

                        _ ->
                            ordered
                )
                []
                (String.lines rawData)
    in
    if List.length matchedOrdered == List.length unOrdered then
        Just <| List.reverse matchedOrdered

    else
        Nothing


parser : Parser Domain
parser =
    succeed
        (\d i ->
            Domain (Label d) (Score i)
        )
        |= getChompedString (chompUntil ".")
        |. chompIf (\c -> not <| Char.isDigit c)
        |. chompWhile (\c -> not <| Char.isDigit c)
        |= int


toString : Domain -> String
toString (Domain (Label l) (Score s)) =
    String.join " " [ l, "–", String.fromInt s ]
