module Main exposing (..)

import Browser
import Debug exposing (..)
import Element
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Github.Object.User as User
import Github.Query as Query
import Graphql.Http
import Graphql.Http.GraphqlError exposing (PossiblyParsedData(..))
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (..)
import Maybe.Extra exposing (combine)
import RemoteData exposing (..)


query : String -> SelectionSet Response RootQuery
query login =
    Query.user { login = login } <|
        SelectionSet.map GithubUser
            User.name


makeRequest : String -> String -> Cmd Msg
makeRequest apiToken login =
    query login
        |> Graphql.Http.queryRequest "https://api.github.com/graphql"
        |> Graphql.Http.withHeader "authorization" ("Bearer" ++ " " ++ apiToken)
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)



---- MODEL ----


type alias Model =
    { apiToken : Maybe String
    , login : Maybe String
    , response : RemoteData (Graphql.Http.Error Response) Response
    }


type alias Response =
    Maybe GithubUser


type alias GithubUser =
    { name : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { apiToken = Maybe.Nothing
      , login = Maybe.Nothing
      , response = RemoteData.NotAsked
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = EnteredApiToken String
    | EnteredLogin String
    | RequestUser
    | GotResponse (RemoteData (Graphql.Http.Error Response) Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredApiToken token ->
            ( { model | apiToken = Just token }, Cmd.none )

        EnteredLogin username ->
            ( { model | login = Just username }, Cmd.none )

        RequestUser ->
            let
                request : Cmd Msg
                request =
                    case Maybe.Extra.combine [ model.apiToken, model.login ] of
                        Just [ apiToken, login ] ->
                            makeRequest apiToken login

                        _ ->
                            Cmd.none
            in
            ( { model | response = RemoteData.Loading }, request )

        GotResponse response ->
            ( { model | response = response }, Cmd.none )



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
                , Element.paddingEach
                    { top = 16
                    , right = 0
                    , bottom = 0
                    , left = 0
                    }
                ]
                [ Element.column
                    [ Element.width Element.fill
                    , Element.spacing 16
                    ]
                    [ Input.text
                        []
                        { onChange = EnteredApiToken
                        , text = Maybe.withDefault "" model.apiToken
                        , placeholder = Input.placeholder [] (Element.text "Enter API Token") |> Just
                        , label = Input.labelAbove [] (Element.text "Github API Token")
                        }
                    , Input.text
                        []
                        { onChange = EnteredLogin
                        , text = Maybe.withDefault "" model.login
                        , placeholder = Input.placeholder [] (Element.text "Enter login") |> Just
                        , label = Input.labelAbove [] (Element.text "Github Login")
                        }
                    , case Maybe.Extra.combine [ model.apiToken, model.login ] of
                        Just [ apiToken, login ] ->
                            Input.button
                                [ Background.color (Element.rgb 238 238 238), Element.centerX ]
                                { onPress = Just RequestUser
                                , label = Element.text "Search"
                                }

                        _ ->
                            Element.none
                    ]
                ]
            , Element.row
                [ Element.centerX
                , Element.centerY
                , Element.height Element.fill
                ]
                [ Element.text <|
                    case model.response of
                        RemoteData.NotAsked ->
                            ""

                        RemoteData.Loading ->
                            "Loading..."

                        RemoteData.Failure err ->
                            case err of
                                Graphql.Http.GraphqlError _ errors ->
                                    List.head errors |> Maybe.map .message |> Maybe.withDefault "Unknown error occurred"

                                _ ->
                                    "Unknown error occured"

                        RemoteData.Success response ->
                            case response of
                                Just user ->
                                    case user.name of
                                        Just name ->
                                            "Name: " ++ name

                                        Nothing ->
                                            "No name"

                                Nothing ->
                                    "User not found"
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
