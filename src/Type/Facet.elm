module Type.Facet exposing (Facet(..), Label(..), Score(..), create, parseList, toString)

import Parser exposing ((|.), (|=), Parser, Step(..), chompUntil, chompWhile, getChompedString, int, loop, map, oneOf, succeed)


type Facet
    = Facet Label Score


type Score
    = Score Int


type Label
    = Label String


parseList : Parser (List Facet)
parseList =
    loop [] parse


parse : List Facet -> Parser (Step (List Facet) (List Facet))
parse facets =
    if List.length facets >= 6 then
        succeed ()
            |> map (\_ -> Done (List.reverse facets))

    else
        oneOf
            [ succeed
                (\s i ->
                    Loop <| create ( s, i ) :: facets
                )
                |. chompWhile (\c -> not <| Char.isAlpha c)
                |= getChompedString (chompUntil ".")
                |. chompWhile (\c -> not <| Char.isDigit c)
                |= int
            , succeed ()
                |> map (\_ -> Done (List.reverse facets))
            ]


create : ( String, Int ) -> Facet
create ( l, s ) =
    Facet (Label l) (Score s)


toString : Facet -> String
toString (Facet (Label l) (Score s)) =
    String.join " " [ l, "–", String.fromInt s ]
