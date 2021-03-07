module Input exposing (Description, Domain(..), DomainLabel(..), DomainScore(..), Facet(..), domains, orderDomains, parse, parseDescriptions)

import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..), chompIf, chompUntil, chompWhile, getChompedString, int, loop, map, oneOf, run, succeed)


type Domain
    = Domain DomainLabel DomainScore


type DomainScore
    = DomainScore Int


type DomainLabel
    = DomainLabel String


type Facet
    = Facet String Int


type alias Description =
    { domain : Domain
    , facets : List Facet
    }


domains : List DomainLabel
domains =
    [ DomainLabel "EXTRAVERSION"
    , DomainLabel "AGREEABLENESS"
    , DomainLabel "CONSCIENTIOUSNESS"
    , DomainLabel "NEUROTICISM"
    , DomainLabel "OPENNESS TO EXPERIENCE"
    ]


orderDomains : List DomainLabel -> String -> Maybe (List DomainLabel)
orderDomains unOrdered rawData =
    let
        matchedOrdered =
            List.foldl
                (\line ordered ->
                    let
                        maybeMatchedLabel : Maybe DomainLabel
                        maybeMatchedLabel =
                            List.foldl
                                (\(DomainLabel label) maybeResult ->
                                    if String.startsWith label line then
                                        Just (DomainLabel label)

                                    else
                                        maybeResult
                                )
                                Nothing
                                unOrdered

                        anyMatch : Bool
                        anyMatch =
                            List.any
                                (\(DomainLabel label) ->
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


parseDescriptions : List ( DomainLabel, String ) -> Result (List DeadEnd) (List Description)
parseDescriptions labelsWithSectionData =
    let
        ( parseErrors, parsedDescriptions ) =
            List.foldl
                (\( label, sectionData ) ( errors, descriptions ) ->
                    case parse (Debug.log "label" label) sectionData of
                        Ok description ->
                            ( errors, description :: descriptions )

                        Err additionalErrrors ->
                            ( additionalErrrors ++ errors, descriptions )
                )
                ( [], [] )
                labelsWithSectionData
    in
    case ( parseErrors, parsedDescriptions ) of
        ( [], descriptions ) ->
            Ok descriptions

        ( errors, _ ) ->
            Err errors


parse : DomainLabel -> String -> Result (List DeadEnd) Description
parse domainLabel rawData =
    run (parseDescription domainLabel) rawData


parseDescription : DomainLabel -> Parser Description
parseDescription (DomainLabel label) =
    succeed
        (\d i facets ->
            Description
                (Domain (DomainLabel d) (DomainScore i))
                facets
        )
        |. chompUntil label
        |= getChompedString (chompUntil ".")
        |. chompIf (\c -> not <| Char.isDigit c)
        |. chompWhile (\c -> not <| Char.isDigit c)
        |= int
        |= parseFacets
        |. chompUntil "\n"


parseFacets : Parser (List Facet)
parseFacets =
    loop [] parseFacet


parseFacet : List Facet -> Parser (Step (List Facet) (List Facet))
parseFacet facets =
    if List.length facets >= 6 then
        succeed ()
            |> map (\_ -> Done (List.reverse facets))

    else
        oneOf
            [ succeed
                (\s i ->
                    Loop (Facet s i :: facets)
                )
                |. chompWhile (\c -> not <| Char.isAlpha c)
                |= getChompedString (chompUntil ".")
                |. chompWhile (\c -> not <| Char.isDigit c)
                |= int
            , succeed ()
                |> map (\_ -> Done (List.reverse facets))
            ]
