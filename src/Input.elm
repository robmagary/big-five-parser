module Input exposing (Description, Domain(..), Facet(..), parse)

import Parser exposing ((|.), (|=), DeadEnd, Parser, chompIf, chompUntil, chompWhile, getChompedString, int, run, succeed)


type Domain
    = Domain String Int


type Facet
    = Facet String Int


type alias Description =
    { domain : Domain
    , facets : List Facet
    }


parse : String -> Result (List DeadEnd) Domain
parse rawData =
    run (parseDomain "EXTRAVERSION") rawData


parseDomain : String -> Parser Domain
parseDomain domain =
    succeed Domain
        |. chompUntil domain
        |= getChompedString (chompUntil ".")
        |. chompIf (\c -> not <| Char.isDigit c)
        |. chompWhile (\c -> not <| Char.isDigit c)
        |= int
