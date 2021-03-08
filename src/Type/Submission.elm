module Type.Submission exposing (Submission, encode, init)

import Json.Encode as Encode
import Type.Description as Description exposing (Description)


type alias Submission =
    { name : String
    , email : String
    , descriptions : List Description
    }


init : List Description -> Submission
init descriptions =
    Submission "" "" descriptions


encode : Submission -> Encode.Value
encode submission =
    Encode.object <|
        [ ( "NAME", Encode.string submission.name )
        , ( "EMAIL", Encode.string submission.email )
        ]
            ++ Description.encodeList submission.descriptions
