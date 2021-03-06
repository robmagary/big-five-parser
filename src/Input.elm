module Input exposing (Description, Domain(..), Facet(..), parse)

import Parser exposing (..)



-- type Domain
--     = Extraversion
--     | Agreeableness
--     | Conscientiousness
--     | Neuroticism
--     | OpennessToExperience


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
    run parseDomain rawData


parseDomain : Parser Domain
parseDomain =
    succeed Domain
        |. chompUntil "EXTRAVERSION"
        |= getChompedString (chompUntil ".")
        |. chompIf (\c -> not <| Char.isDigit c)
        |. chompWhile (\c -> not <| Char.isDigit c)
        |= int



-- succeed Domain
--     |. chompUntil "Domain/Facet...... Score"
--     |. chompWhile Char.isAlpha
--     |= getChompedString (chompUntil ".")
