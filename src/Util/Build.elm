module Util.Build exposing (add, addIf, addMaybeMap, concat)


add : a -> List a -> List a
add value list =
    List.append list [ value ]


addIf : Bool -> a -> List a -> List a
addIf condition value list =
    if condition then
        add value list

    else
        list


addMaybeMap : (b -> a) -> Maybe b -> List a -> List a
addMaybeMap fn maybe list =
    case Maybe.map fn maybe of
        Just toAdd ->
            add toAdd list

        Nothing ->
            list


concat : List a -> List a -> List a
concat toConcat list =
    list ++ toConcat
