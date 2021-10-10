port module Main exposing (..)

import Browser
import Debug exposing (todo)
import Element exposing (Element, text)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font
import Element.Input as Input
import Github.Enum.RepositoryContributionType exposing (RepositoryContributionType(..))
import Github.Interface.Actor exposing (avatarUrl)
import Github.Object
import Github.Object.ContributionsCollection
import Github.Object.FollowerConnection
import Github.Object.IssueConnection
import Github.Object.PullRequestConnection
import Github.Object.RepositoryConnection
import Github.Object.User as User
import Github.Query as Query
import Github.Scalar exposing (Uri)
import Graphql.Http
import Graphql.Http.GraphqlError exposing (PossiblyParsedData(..))
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (..)
import Maybe.Extra exposing (combine)
import RemoteData exposing (..)


followersFragment : SelectionSet Int Github.Object.User
followersFragment =
    User.followers identity Github.Object.FollowerConnection.totalCount


pullRequestsFragment : SelectionSet Int Github.Object.User
pullRequestsFragment =
    User.pullRequests identity Github.Object.PullRequestConnection.totalCount


issuesFragment : SelectionSet Int Github.Object.User
issuesFragment =
    User.issues identity Github.Object.IssueConnection.totalCount


repositoriesFragment : SelectionSet Int Github.Object.User
repositoriesFragment =
    User.repositoriesContributedTo
        (\optionals ->
            { optionals
                | contributionTypes = Present [ Just Commit, Just Issue, Just PullRequest, Just Repository ]
            }
        )
        Github.Object.RepositoryConnection.totalCount


contributionsFragment : SelectionSet Int Github.Object.User
contributionsFragment =
    User.contributionsCollection identity Github.Object.ContributionsCollection.totalCommitContributions


query : String -> SelectionSet Response RootQuery
query login =
    Query.user { login = login } <|
        SelectionSet.map7 GithubUser
            User.name
            (User.avatarUrl
                (\optionals ->
                    { optionals
                        | size = Present avatarSize
                    }
                )
            )
            followersFragment
            pullRequestsFragment
            issuesFragment
            repositoriesFragment
            contributionsFragment


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
    , avatarUrl : Github.Scalar.Uri
    , followers : Int
    , pullRequests : Int
    , issues : Int
    , repositories : Int
    , commits : Int
    }


init : String -> ( Model, Cmd Msg )
init flags =
    ( { apiToken = nonEmptyString flags
      , login = Nothing
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
            ( { model | apiToken = nonEmptyString token }
            , saveToken token
            )

        EnteredLogin username ->
            ( { model | login = nonEmptyString username }, Cmd.none )

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


viewUsernameForm : Maybe String -> Maybe String -> List (Element.Element Msg)
viewUsernameForm apiToken login =
    [ Input.text
        []
        { onChange = EnteredApiToken
        , text = Maybe.withDefault "" apiToken
        , placeholder = Input.placeholder [] (Element.text "Enter API Token") |> Just
        , label = Input.labelAbove [] (Element.text "Github API Token")
        }
    , Input.text
        []
        { onChange = EnteredLogin
        , text = Maybe.withDefault "" login
        , placeholder = Input.placeholder [] (Element.text "Enter login") |> Just
        , label = Input.labelAbove [] (Element.text "Github Login")
        }
    , [ apiToken, login ]
        |> Maybe.Extra.combine
        |> Maybe.map
            (\_ ->
                Input.button
                    [ Background.color (Element.rgb 238 238 238), Element.centerX ]
                    { onPress = Just RequestUser
                    , label = Element.text "Search"
                    }
            )
        |> Maybe.withDefault Element.none
    ]


viewAvatar : Github.Scalar.Uri -> Element msg
viewAvatar (Github.Scalar.Uri avatarUrl) =
    Element.image
        [ Element.centerX
        , Element.height <| Element.px avatarSize
        , Element.width <| Element.px avatarSize
        , Element.clip
        , Border.rounded <| (avatarSize // 2)
        ]
        { src = avatarUrl
        , description = "Github avatar"
        }


viewResult : GithubUser -> Element.Element Msg
viewResult user =
    Element.column
        [ Element.spacing 8 ]
        [ viewAvatar user.avatarUrl
        , Element.text <|
            (++) "Name: " <|
                Maybe.withDefault "No name" user.name
        , Element.text <| "Followers: " ++ String.fromInt user.followers
        , Element.text <| "Repositories: " ++ String.fromInt user.repositories
        , Element.text <| "Commits: " ++ String.fromInt user.commits
        , Element.text <| "Pull Requets: " ++ String.fromInt user.pullRequests
        , Element.text <| "Issues: " ++ String.fromInt user.issues
        ]


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
                    (viewUsernameForm model.apiToken model.login)
                ]
            , Element.row
                [ Element.centerX
                , Element.centerY
                , Element.height Element.fill
                ]
                [ case model.response of
                    RemoteData.NotAsked ->
                        Element.text ""

                    RemoteData.Loading ->
                        Element.text "Loading..."

                    RemoteData.Failure err ->
                        case err of
                            Graphql.Http.GraphqlError _ errors ->
                                Element.text <|
                                    (List.head errors
                                        |> Maybe.map .message
                                        |> Maybe.withDefault "Unknown error occurred"
                                    )

                            _ ->
                                Element.text "Unknown error occured"

                    RemoteData.Success response_ ->
                        Maybe.map viewResult response_
                            |> Maybe.withDefault (Element.text "User not found")
                ]
            ]


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Github Stats"
    , body = [ viewBody model ]
    }



---- PROGRAM ----


main : Program String Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



---- PORTS ----


port saveToken : String -> Cmd msg


port loadToken : (Maybe String -> msg) -> Sub msg



---- UTILITY ----


avatarSize : Int
avatarSize =
    100


nonEmptyString : String -> Maybe String
nonEmptyString str =
    if String.isEmpty str then
        Nothing

    else
        Just str
