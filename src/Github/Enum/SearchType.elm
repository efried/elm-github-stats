-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Enum.SearchType exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| Represents the individual results of a search.

  - Issue - Returns results matching issues in repositories.
  - Repository - Returns results matching repositories.
  - User - Returns results matching users and organizations on GitHub.
  - Discussion - Returns matching discussions in repositories.

-}
type SearchType
    = Issue
    | Repository
    | User
    | Discussion


list : List SearchType
list =
    [ Issue, Repository, User, Discussion ]


decoder : Decoder SearchType
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ISSUE" ->
                        Decode.succeed Issue

                    "REPOSITORY" ->
                        Decode.succeed Repository

                    "USER" ->
                        Decode.succeed User

                    "DISCUSSION" ->
                        Decode.succeed Discussion

                    _ ->
                        Decode.fail ("Invalid SearchType type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : SearchType -> String
toString enum____ =
    case enum____ of
        Issue ->
            "ISSUE"

        Repository ->
            "REPOSITORY"

        User ->
            "USER"

        Discussion ->
            "DISCUSSION"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe SearchType
fromString enumString____ =
    case enumString____ of
        "ISSUE" ->
            Just Issue

        "REPOSITORY" ->
            Just Repository

        "USER" ->
            Just User

        "DISCUSSION" ->
            Just Discussion

        _ ->
            Nothing
