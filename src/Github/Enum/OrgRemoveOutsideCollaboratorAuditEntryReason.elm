-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Enum.OrgRemoveOutsideCollaboratorAuditEntryReason exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The reason an outside collaborator was removed from an Organization.

  - TwoFactorRequirementNonCompliance - The organization required 2FA of its billing managers and this user did not have 2FA enabled.
  - SamlExternalIdentityMissing - SAML external identity missing

-}
type OrgRemoveOutsideCollaboratorAuditEntryReason
    = TwoFactorRequirementNonCompliance
    | SamlExternalIdentityMissing


list : List OrgRemoveOutsideCollaboratorAuditEntryReason
list =
    [ TwoFactorRequirementNonCompliance, SamlExternalIdentityMissing ]


decoder : Decoder OrgRemoveOutsideCollaboratorAuditEntryReason
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "TWO_FACTOR_REQUIREMENT_NON_COMPLIANCE" ->
                        Decode.succeed TwoFactorRequirementNonCompliance

                    "SAML_EXTERNAL_IDENTITY_MISSING" ->
                        Decode.succeed SamlExternalIdentityMissing

                    _ ->
                        Decode.fail ("Invalid OrgRemoveOutsideCollaboratorAuditEntryReason type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : OrgRemoveOutsideCollaboratorAuditEntryReason -> String
toString enum____ =
    case enum____ of
        TwoFactorRequirementNonCompliance ->
            "TWO_FACTOR_REQUIREMENT_NON_COMPLIANCE"

        SamlExternalIdentityMissing ->
            "SAML_EXTERNAL_IDENTITY_MISSING"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe OrgRemoveOutsideCollaboratorAuditEntryReason
fromString enumString____ =
    case enumString____ of
        "TWO_FACTOR_REQUIREMENT_NON_COMPLIANCE" ->
            Just TwoFactorRequirementNonCompliance

        "SAML_EXTERNAL_IDENTITY_MISSING" ->
            Just SamlExternalIdentityMissing

        _ ->
            Nothing
