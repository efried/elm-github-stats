-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Enum.DeploymentReviewState exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The possible states for a deployment review.

  - Approved - The deployment was approved.
  - Rejected - The deployment was rejected.

-}
type DeploymentReviewState
    = Approved
    | Rejected


list : List DeploymentReviewState
list =
    [ Approved, Rejected ]


decoder : Decoder DeploymentReviewState
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "APPROVED" ->
                        Decode.succeed Approved

                    "REJECTED" ->
                        Decode.succeed Rejected

                    _ ->
                        Decode.fail ("Invalid DeploymentReviewState type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : DeploymentReviewState -> String
toString enum____ =
    case enum____ of
        Approved ->
            "APPROVED"

        Rejected ->
            "REJECTED"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe DeploymentReviewState
fromString enumString____ =
    case enumString____ of
        "APPROVED" ->
            Just Approved

        "REJECTED" ->
            Just Rejected

        _ ->
            Nothing
