module InputSpec exposing (suite)

import Expect
import Input exposing (parse)
import Test exposing (Test, describe, test)
import TestData


suite : Test
suite =
    describe "Parsing Domains"
        [ test "Single domain data parses EXTRAVERSION 64"
            (\_ ->
                Expect.equal
                    (Ok [ Input.Domain "EXTRAVERSION" 64 ])
                    (parse TestData.singleDomainData)
            )
        , test "Multiple domains data parses EXTRAVERSION 64 and AGREEABLENESS 66"
            (\_ ->
                Expect.equal
                    (Ok
                        [ Input.Domain "EXTRAVERSION" 64
                        , Input.Domain "AGREEABLENESS" 66
                        ]
                    )
                    (parse TestData.multipleDomains)
            )
        ]
