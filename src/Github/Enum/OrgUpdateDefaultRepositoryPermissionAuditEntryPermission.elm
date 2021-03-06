-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Enum.OrgUpdateDefaultRepositoryPermissionAuditEntryPermission exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The default permission a repository can have in an Organization.

  - Read - Can read and clone repositories.
  - Write - Can read, clone and push to repositories.
  - Admin - Can read, clone, push, and add collaborators to repositories.
  - None - No default permission value.

-}
type OrgUpdateDefaultRepositoryPermissionAuditEntryPermission
    = Read
    | Write
    | Admin
    | None


list : List OrgUpdateDefaultRepositoryPermissionAuditEntryPermission
list =
    [ Read, Write, Admin, None ]


decoder : Decoder OrgUpdateDefaultRepositoryPermissionAuditEntryPermission
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "READ" ->
                        Decode.succeed Read

                    "WRITE" ->
                        Decode.succeed Write

                    "ADMIN" ->
                        Decode.succeed Admin

                    "NONE" ->
                        Decode.succeed None

                    _ ->
                        Decode.fail ("Invalid OrgUpdateDefaultRepositoryPermissionAuditEntryPermission type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : OrgUpdateDefaultRepositoryPermissionAuditEntryPermission -> String
toString enum____ =
    case enum____ of
        Read ->
            "READ"

        Write ->
            "WRITE"

        Admin ->
            "ADMIN"

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
fromString : String -> Maybe OrgUpdateDefaultRepositoryPermissionAuditEntryPermission
fromString enumString____ =
    case enumString____ of
        "READ" ->
            Just Read

        "WRITE" ->
            Just Write

        "ADMIN" ->
            Just Admin

        "NONE" ->
            Just None

        _ ->
            Nothing
