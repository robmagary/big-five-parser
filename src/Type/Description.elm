module Type.Description exposing (Description, encodeList, parse, parseList, parser)

import Json.Encode as Encode
import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..), chompUntil, run, succeed)
import Type.Domain as Domain exposing (Domain(..))
import Type.Facet as Facet exposing (Facet(..))


type alias Description =
    { domain : Domain
    , facets : List Facet
    }


parseList : List ( Domain.Label, String ) -> Result (List DeadEnd) (List Description)
parseList labelsWithSectionData =
    let
        ( parseErrors, parsedDescriptions ) =
            List.foldr
                (\( label, sectionData ) ( errors, descriptions ) ->
                    case parse label sectionData of
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


parse : Domain.Label -> String -> Result (List DeadEnd) Description
parse domainLabel rawData =
    run (parser domainLabel) rawData


parser : Domain.Label -> Parser Description
parser (Domain.Label label) =
    succeed
        (\d facets ->
            Description d facets
        )
        |. chompUntil label
        |= Domain.parser
        |= Facet.parseList
        |. chompUntil "\n"


encodeList : List Description -> List ( String, Encode.Value )
encodeList descriptions =
    List.map
        (\{ domain, facets } ->
            Domain.encode domain facets
        )
        descriptions
