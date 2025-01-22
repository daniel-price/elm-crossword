module Util.List exposing (getNextItem)

import List.Extra


dropUntilMember : a -> List a -> List a
dropUntilMember item list =
    case List.Extra.findIndex ((==) item) list of
        Just index ->
            List.drop (index + 1) list

        Nothing ->
            list


startingAtMember : a -> List a -> List a
startingAtMember item list =
    case List.Extra.findIndex ((==) item) list of
        Just index ->
            List.drop (index + 1) list
                ++ List.take index list

        Nothing ->
            list


getNextItem : Bool -> a -> List a -> a
getNextItem loopList currentItem list =
    list
        |> (if loopList then
                startingAtMember currentItem

            else
                dropUntilMember currentItem
           )
        |> List.head
        |> Maybe.withDefault currentItem
