module Input exposing (Description, Domain(..), Facet(..), parse)

import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..), andThen, chompIf, chompUntil, chompWhile, getChompedString, int, loop, map, oneOf, run, succeed)


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
    , "OPENNESS TO EXPERIENCE"
    , "NEUROTICISM"
    , "AGREEABLENESS"
    , "CONSCIENTIOUSNESS"
    ]


parse : String -> Result (List DeadEnd) (List Domain)
parse rawData =
    run (parseDomains rawData) rawData


parseDomains : String -> Parser (List Domain)
parseDomains rawData =
    loop [] (parseDomain rawData)


parseDomain : String -> List Domain -> Parser (Step (List Domain) (List Domain))
parseDomain rawData parsedDomains =
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
                    succeed (\d i -> Loop <| List.reverse (Domain d i :: parsedDomains))
                        |. chompUntil domainString
                        |= getChompedString (chompUntil ".")
                        |. chompIf (\c -> not <| Char.isDigit c)
                        |. chompWhile (\c -> not <| Char.isDigit c)
                        |= int
                )
                orderedDomains

        doneAtTheEnd =
            succeed () |> map (\_ -> Done parsedDomains)
    in
    oneOf
        (List.reverse <|
            doneAtTheEnd
                :: domainParserList
        )
