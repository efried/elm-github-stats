-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Enum.EnterpriseAdministratorRole exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The possible administrator roles in an enterprise account.

  - Owner - Represents an owner of the enterprise account.
  - BillingManager - Represents a billing manager of the enterprise account.

-}
type EnterpriseAdministratorRole
    = Owner
    | BillingManager


list : List EnterpriseAdministratorRole
list =
    [ Owner, BillingManager ]


decoder : Decoder EnterpriseAdministratorRole
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "OWNER" ->
                        Decode.succeed Owner

                    "BILLING_MANAGER" ->
                        Decode.succeed BillingManager

                    _ ->
                        Decode.fail ("Invalid EnterpriseAdministratorRole type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : EnterpriseAdministratorRole -> String
toString enum____ =
    case enum____ of
        Owner ->
            "OWNER"

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
fromString : String -> Maybe EnterpriseAdministratorRole
fromString enumString____ =
    case enumString____ of
        "OWNER" ->
            Just Owner

        "BILLING_MANAGER" ->
            Just BillingManager

        _ ->
            Nothing
