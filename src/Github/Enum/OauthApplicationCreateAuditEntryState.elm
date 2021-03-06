-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Enum.OauthApplicationCreateAuditEntryState exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The state of an OAuth Application when it was created.

  - Active - The OAuth Application was active and allowed to have OAuth Accesses.
  - Suspended - The OAuth Application was suspended from generating OAuth Accesses due to abuse or security concerns.
  - PendingDeletion - The OAuth Application was in the process of being deleted.

-}
type OauthApplicationCreateAuditEntryState
    = Active
    | Suspended
    | PendingDeletion


list : List OauthApplicationCreateAuditEntryState
list =
    [ Active, Suspended, PendingDeletion ]


decoder : Decoder OauthApplicationCreateAuditEntryState
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ACTIVE" ->
                        Decode.succeed Active

                    "SUSPENDED" ->
                        Decode.succeed Suspended

                    "PENDING_DELETION" ->
                        Decode.succeed PendingDeletion

                    _ ->
                        Decode.fail ("Invalid OauthApplicationCreateAuditEntryState type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : OauthApplicationCreateAuditEntryState -> String
toString enum____ =
    case enum____ of
        Active ->
            "ACTIVE"

        Suspended ->
            "SUSPENDED"

        PendingDeletion ->
            "PENDING_DELETION"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe OauthApplicationCreateAuditEntryState
fromString enumString____ =
    case enumString____ of
        "ACTIVE" ->
            Just Active

        "SUSPENDED" ->
            Just Suspended

        "PENDING_DELETION" ->
            Just PendingDeletion

        _ ->
            Nothing
