module Type.Submission exposing (Submission, init)

import Type.Description exposing (Description)


type alias Submission =
    { name : String
    , email : String
    , descriptions : List Description
    }


init : List Description -> Submission
init descriptions =
    Submission "" "" descriptions
