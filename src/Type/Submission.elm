module Type.Submission exposing (Submission, decode, encode, expectReceiptOrMessage, init, postUrl)

import Http
import Json.Decode as Decode
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


decode : Decode.Decoder String
decode =
    Decode.string


expectReceiptOrMessage : (Result Http.Error String -> msg) -> Decode.Decoder String -> Http.Expect msg
expectReceiptOrMessage toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode == 422 then
                        case Decode.decodeString decoder (Debug.log "422 body" body) of
                            Ok value ->
                                Ok value

                            Err err ->
                                Err (Http.BadBody (Decode.errorToString err))

                    else
                        Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (Decode.errorToString err))


postUrl : String
postUrl =
    "https://recruitbot.trikeapps.com/api/v1/roles/bellroy-tech-team-recruit/big_five_profile_submissions"
