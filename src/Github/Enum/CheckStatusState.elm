-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Enum.CheckStatusState exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The possible states for a check suite or run status.

  - Queued - The check suite or run has been queued.
  - InProgress - The check suite or run is in progress.
  - Completed - The check suite or run has been completed.
  - Waiting - The check suite or run is in waiting state.
  - Pending - The check suite or run is in pending state.
  - Requested - The check suite or run has been requested.

-}
type CheckStatusState
    = Queued
    | InProgress
    | Completed
    | Waiting
    | Pending
    | Requested


list : List CheckStatusState
list =
    [ Queued, InProgress, Completed, Waiting, Pending, Requested ]


decoder : Decoder CheckStatusState
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "QUEUED" ->
                        Decode.succeed Queued

                    "IN_PROGRESS" ->
                        Decode.succeed InProgress

                    "COMPLETED" ->
                        Decode.succeed Completed

                    "WAITING" ->
                        Decode.succeed Waiting

                    "PENDING" ->
                        Decode.succeed Pending

                    "REQUESTED" ->
                        Decode.succeed Requested

                    _ ->
                        Decode.fail ("Invalid CheckStatusState type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : CheckStatusState -> String
toString enum____ =
    case enum____ of
        Queued ->
            "QUEUED"

        InProgress ->
            "IN_PROGRESS"

        Completed ->
            "COMPLETED"

        Waiting ->
            "WAITING"

        Pending ->
            "PENDING"

        Requested ->
            "REQUESTED"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe CheckStatusState
fromString enumString____ =
    case enumString____ of
        "QUEUED" ->
            Just Queued

        "IN_PROGRESS" ->
            Just InProgress

        "COMPLETED" ->
            Just Completed

        "WAITING" ->
            Just Waiting

        "PENDING" ->
            Just Pending

        "REQUESTED" ->
            Just Requested

        _ ->
            Nothing
