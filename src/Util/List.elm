module Util.List exposing (dropUntilMember)

import List.Extra


dropUntilMember : a -> List a -> List a
dropUntilMember item list =
    case List.Extra.findIndex ((==) item) list of
        Just index ->
            List.drop (index + 1) list

        Nothing ->
            list
