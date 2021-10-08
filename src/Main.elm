module Main exposing (..)

import Browser
import Element
import Element.Font as Font
import Element.Input as Input
import Html exposing (..)



---- MODEL ----


type alias Model =
    { apiToken : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { apiToken = Maybe.Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = EnteredApiToken String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredApiToken token ->
            ( { model | apiToken = Just token }, Cmd.none )



---- VIEW ----


viewBody : Model -> Html Msg
viewBody model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Font.family
            [ Font.typeface "Helvetica"
            , Font.sansSerif
            ]
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.row
                [ Element.centerX
                , Element.height (Element.fillPortion 1)
                ]
                [ Input.text
                    []
                    { onChange = EnteredApiToken
                    , text = Maybe.withDefault "" model.apiToken
                    , placeholder = Input.placeholder [] (Element.text "Enter Github API Token") |> Just
                    , label = Input.labelAbove [] (Element.text "API Token")
                    }
                ]
            , Element.row
                [ Element.centerX
                , Element.height (Element.fillPortion 5)
                ]
                [ Element.text "Graphic will go here"
                ]
            ]


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Github Stats"
    , body = [ viewBody model ]
    }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
