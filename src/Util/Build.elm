module Util.Build exposing (add, addMaybeMap)


add : a -> List a -> List a
add value list =
    List.append list [ value ]


addMaybeMap : (b -> a) -> Maybe b -> List a -> List a
addMaybeMap fn maybe list =
    case Maybe.map fn maybe of
        Just toAdd ->
            add toAdd list

        Nothing ->
            list
