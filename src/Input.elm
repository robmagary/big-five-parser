module Input exposing (Description, Domain(..), Facet(..), parse)

import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..), andThen, chompIf, chompUntil, chompUntilEndOr, chompWhile, getChompedString, int, loop, map, oneOf, run, succeed)


type Domain
    = Domain String Int


type Facet
    = Facet String Int


type alias Description =
    { domain : Domain
    , facets : List Facet
    }


domains : List String
domains =
    [ "EXTRAVERSION"
    , "AGREEABLENESS"
    , "CONSCIENTIOUSNESS"
    , "NEUROTICISM"
    , "OPENNESS TO EXPERIENCE"
    ]


parse : String -> Result (List DeadEnd) (List Description)
parse rawData =
    run (parseDescriptions rawData) rawData


parseDescriptions : String -> Parser (List Description)
parseDescriptions rawData =
    loop [] (parseDescription rawData)


parseDescription : String -> List Description -> Parser (Step (List Description) (List Description))
parseDescription rawData parsedDescriptions =
    let
        orderedDomains =
            List.foldl
                (\line ordered ->
                    let
                        matchingDomain =
                            List.foldl
                                (\domain matchedDomain ->
                                    if String.startsWith domain line then
                                        domain

                                    else
                                        matchedDomain
                                )
                                ""
                                domains
                    in
                    if List.any (\domain -> String.startsWith domain line) domains then
                        matchingDomain :: ordered

                    else
                        ordered
                )
                []
                (String.lines rawData)

        domainParserList =
            List.map
                (\domainString ->
                    succeed
                        (\d i facets -> Loop (Description (Domain d i) facets :: parsedDescriptions))
                        |. chompUntil domainString
                        |= getChompedString (chompUntil ".")
                        |. chompIf (\c -> not <| Char.isDigit c)
                        |. chompWhile (\c -> not <| Char.isDigit c)
                        |= int
                        |= parseFacets
                )
                orderedDomains

        doneAtTheEnd =
            succeed () |> map (\_ -> Done <| List.reverse parsedDescriptions)
    in
    oneOf
        (List.reverse <|
            doneAtTheEnd
                :: domainParserList
        )


parseFacets : Parser (List Facet)
parseFacets =
    loop [] parseFacet


parseFacet : List Facet -> Parser (Step (List Facet) (List Facet))
parseFacet facets =
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
