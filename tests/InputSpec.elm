module InputSpec exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import TestData
import Type.Description as Description exposing (Description)
import Type.Domain as Domain exposing (Domain(..))
import Type.Facet as Facet exposing (Facet(..))


suite : Test
suite =
    describe "Parsing Descriptions"
        [ test "Single domain data parses EXTRAVERSION 64 and 6 Facets"
            (\_ ->
                let
                    facetResults =
                        [ Facet.create ( "Friendliness", 75 )
                        , Facet.create ( "Gregariousness", 64 )
                        , Facet.create ( "Assertiveness", 50 )
                        , Facet.create ( "Activity Level", 57 )
                        , Facet.create ( "Excitement-Seeking", 41 )
                        , Facet.create ( "Cheerfulness", 67 )
                        ]
                in
                Expect.equal
                    (Ok <| Description (Domain (Domain.Label "EXTRAVERSION") (Domain.Score 64)) facetResults)
                    (Description.parse (Domain.Label "EXTRAVERSION") TestData.singleDomainData)
            )
        ]
