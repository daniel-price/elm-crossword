module Util.List exposing (getNextItem)

import List.Extra


dropUntilMember : a -> List a -> List a
dropUntilMember item list =
    case List.Extra.findIndex ((==) item) list of
        Just index ->
            List.drop (index + 1) list

        Nothing ->
            list


getNextItem : a -> List a -> a
getNextItem currentItem list =
    list
        |> dropUntilMember currentItem
        |> List.head
        |> Maybe.withDefault currentItem
