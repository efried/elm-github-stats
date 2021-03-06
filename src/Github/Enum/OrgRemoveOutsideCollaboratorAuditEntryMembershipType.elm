-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Enum.OrgRemoveOutsideCollaboratorAuditEntryMembershipType exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The type of membership a user has with an Organization.

  - OutsideCollaborator - An outside collaborator is a person who isn't explicitly a member of the Organization, but who has Read, Write, or Admin permissions to one or more repositories in the organization.
  - Unaffiliated - An unaffiliated collaborator is a person who is not a member of the Organization and does not have access to any repositories in the organization.
  - BillingManager - A billing manager is a user who manages the billing settings for the Organization, such as updating payment information.

-}
type OrgRemoveOutsideCollaboratorAuditEntryMembershipType
    = OutsideCollaborator
    | Unaffiliated
    | BillingManager


list : List OrgRemoveOutsideCollaboratorAuditEntryMembershipType
list =
    [ OutsideCollaborator, Unaffiliated, BillingManager ]


decoder : Decoder OrgRemoveOutsideCollaboratorAuditEntryMembershipType
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "OUTSIDE_COLLABORATOR" ->
                        Decode.succeed OutsideCollaborator

                    "UNAFFILIATED" ->
                        Decode.succeed Unaffiliated

                    "BILLING_MANAGER" ->
                        Decode.succeed BillingManager

                    _ ->
                        Decode.fail ("Invalid OrgRemoveOutsideCollaboratorAuditEntryMembershipType type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : OrgRemoveOutsideCollaboratorAuditEntryMembershipType -> String
toString enum____ =
    case enum____ of
        OutsideCollaborator ->
            "OUTSIDE_COLLABORATOR"

        Unaffiliated ->
            "UNAFFILIATED"

        BillingManager ->
            "BILLING_MANAGER"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe OrgRemoveOutsideCollaboratorAuditEntryMembershipType
fromString enumString____ =
    case enumString____ of
        "OUTSIDE_COLLABORATOR" ->
            Just OutsideCollaborator

        "UNAFFILIATED" ->
            Just Unaffiliated

        "BILLING_MANAGER" ->
            Just BillingManager

        _ ->
            Nothing
