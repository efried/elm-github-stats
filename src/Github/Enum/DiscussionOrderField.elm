-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Enum.DiscussionOrderField exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| Properties by which discussion connections can be ordered.

  - CreatedAt - Order discussions by creation time.
  - UpdatedAt - Order discussions by most recent modification time.

-}
type DiscussionOrderField
    = CreatedAt
    | UpdatedAt


list : List DiscussionOrderField
list =
    [ CreatedAt, UpdatedAt ]


decoder : Decoder DiscussionOrderField
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "CREATED_AT" ->
                        Decode.succeed CreatedAt

                    "UPDATED_AT" ->
                        Decode.succeed UpdatedAt

                    _ ->
                        Decode.fail ("Invalid DiscussionOrderField type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : DiscussionOrderField -> String
toString enum____ =
    case enum____ of
        CreatedAt ->
            "CREATED_AT"

        UpdatedAt ->
            "UPDATED_AT"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe DiscussionOrderField
fromString enumString____ =
    case enumString____ of
        "CREATED_AT" ->
            Just CreatedAt

        "UPDATED_AT" ->
            Just UpdatedAt

        _ ->
            Nothing
