module JsonApi.OneOrMany exposing (..)


type OneOrMany a
    = None
    | One a
    | Many (List a)


map : (a -> b) -> OneOrMany a -> OneOrMany b
map fn oneOrMany =
    case oneOrMany of
        None ->
            None
        One x ->
            One (fn x)

        Many xs ->
            Many (List.map fn xs)


extractOne : OneOrMany a -> Result String a
extractOne oneOrMany =
    case oneOrMany of
        None ->
            Err "Expected a singleton resource, got none"

        One x ->
            Ok x

        Many xs ->
            Err "Expected a singleton resource, got a collection"

extractOneOrNone : OneOrMany a -> Result String (Maybe a)
extractOneOrNone oneOrMany =
    case oneOrMany of
        None ->
            Ok Nothing

        One x ->
            Ok (Just x)

        Many xs ->
            Err "Expected nothing or a singleton, got a collection"

extractMany : OneOrMany a -> Result String (List a)
extractMany oneOrMany =
    case oneOrMany of
        None ->
            Err "Expected a collection of resources, got none"

        One x ->
            Err "Expected a collection of resources, got a singleton"

        Many xs ->
            Ok xs
