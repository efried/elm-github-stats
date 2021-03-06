-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Enum.SponsorsActivityPeriod exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The possible time periods for which Sponsors activities can be requested.

  - Day - The previous calendar day.
  - Week - The previous seven days.
  - Month - The previous thirty days.
  - All - Don't restrict the activity to any date range, include all activity.

-}
type SponsorsActivityPeriod
    = Day
    | Week
    | Month
    | All


list : List SponsorsActivityPeriod
list =
    [ Day, Week, Month, All ]


decoder : Decoder SponsorsActivityPeriod
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "DAY" ->
                        Decode.succeed Day

                    "WEEK" ->
                        Decode.succeed Week

                    "MONTH" ->
                        Decode.succeed Month

                    "ALL" ->
                        Decode.succeed All

                    _ ->
                        Decode.fail ("Invalid SponsorsActivityPeriod type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : SponsorsActivityPeriod -> String
toString enum____ =
    case enum____ of
        Day ->
            "DAY"

        Week ->
            "WEEK"

        Month ->
            "MONTH"

        All ->
            "ALL"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe SponsorsActivityPeriod
fromString enumString____ =
    case enumString____ of
        "DAY" ->
            Just Day

        "WEEK" ->
            Just Week

        "MONTH" ->
            Just Month

        "ALL" ->
            Just All

        _ ->
            Nothing
