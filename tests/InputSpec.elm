module InputSpec exposing (suite)

import Expect
import Input exposing (DomainLabel(..), DomainScore(..), Facet(..), parse)
import Test exposing (Test, describe, test)
import TestData


suite : Test
suite =
    describe "Parsing Descriptions"
        [ test "Single domain data parses EXTRAVERSION 64 and 6 Facets"
            (\_ ->
                let
                    facetResults =
                        [ Facet "Friendliness" 75
                        , Facet "Gregariousness" 64
                        , Facet "Assertiveness" 50
                        , Facet "Activity Level" 57
                        , Facet "Excitement-Seeking" 41
                        , Facet "Cheerfulness" 67
                        ]
                in
                Expect.equal
                    (Ok <| Input.Description (Input.Domain (DomainLabel "EXTRAVERSION") (DomainScore 64)) facetResults)
                    (parse (DomainLabel "EXTRAVERSION") TestData.singleDomainData)
            )
        ]
