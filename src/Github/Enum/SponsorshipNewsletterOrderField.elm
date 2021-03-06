-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Enum.SponsorshipNewsletterOrderField exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| Properties by which sponsorship update connections can be ordered.

  - CreatedAt - Order sponsorship newsletters by when they were created.

-}
type SponsorshipNewsletterOrderField
    = CreatedAt


list : List SponsorshipNewsletterOrderField
list =
    [ CreatedAt ]


decoder : Decoder SponsorshipNewsletterOrderField
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "CREATED_AT" ->
                        Decode.succeed CreatedAt

                    _ ->
                        Decode.fail ("Invalid SponsorshipNewsletterOrderField type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : SponsorshipNewsletterOrderField -> String
toString enum____ =
    case enum____ of
        CreatedAt ->
            "CREATED_AT"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe SponsorshipNewsletterOrderField
fromString enumString____ =
    case enumString____ of
        "CREATED_AT" ->
            Just CreatedAt

        _ ->
            Nothing
