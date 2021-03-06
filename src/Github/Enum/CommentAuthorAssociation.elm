-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Enum.CommentAuthorAssociation exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| A comment author association with repository.

  - Member - Author is a member of the organization that owns the repository.
  - Owner - Author is the owner of the repository.
  - Mannequin - Author is a placeholder for an unclaimed user.
  - Collaborator - Author has been invited to collaborate on the repository.
  - Contributor - Author has previously committed to the repository.
  - FirstTimeContributor - Author has not previously committed to the repository.
  - FirstTimer - Author has not previously committed to GitHub.
  - None - Author has no association with the repository.

-}
type CommentAuthorAssociation
    = Member
    | Owner
    | Mannequin
    | Collaborator
    | Contributor
    | FirstTimeContributor
    | FirstTimer
    | None


list : List CommentAuthorAssociation
list =
    [ Member, Owner, Mannequin, Collaborator, Contributor, FirstTimeContributor, FirstTimer, None ]


decoder : Decoder CommentAuthorAssociation
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "MEMBER" ->
                        Decode.succeed Member

                    "OWNER" ->
                        Decode.succeed Owner

                    "MANNEQUIN" ->
                        Decode.succeed Mannequin

                    "COLLABORATOR" ->
                        Decode.succeed Collaborator

                    "CONTRIBUTOR" ->
                        Decode.succeed Contributor

                    "FIRST_TIME_CONTRIBUTOR" ->
                        Decode.succeed FirstTimeContributor

                    "FIRST_TIMER" ->
                        Decode.succeed FirstTimer

                    "NONE" ->
                        Decode.succeed None

                    _ ->
                        Decode.fail ("Invalid CommentAuthorAssociation type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : CommentAuthorAssociation -> String
toString enum____ =
    case enum____ of
        Member ->
            "MEMBER"

        Owner ->
            "OWNER"

        Mannequin ->
            "MANNEQUIN"

        Collaborator ->
            "COLLABORATOR"

        Contributor ->
            "CONTRIBUTOR"

        FirstTimeContributor ->
            "FIRST_TIME_CONTRIBUTOR"

        FirstTimer ->
            "FIRST_TIMER"

        None ->
            "NONE"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe CommentAuthorAssociation
fromString enumString____ =
    case enumString____ of
        "MEMBER" ->
            Just Member

        "OWNER" ->
            Just Owner

        "MANNEQUIN" ->
            Just Mannequin

        "COLLABORATOR" ->
            Just Collaborator

        "CONTRIBUTOR" ->
            Just Contributor

        "FIRST_TIME_CONTRIBUTOR" ->
            Just FirstTimeContributor

        "FIRST_TIMER" ->
            Just FirstTimer

        "NONE" ->
            Just None

        _ ->
            Nothing
