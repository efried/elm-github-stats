port module Main exposing (..)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Github.Enum.OrderDirection exposing (OrderDirection(..))
import Github.Enum.RepositoryAffiliation exposing (RepositoryAffiliation(..))
import Github.Enum.RepositoryContributionType exposing (RepositoryContributionType(..))
import Github.Enum.RepositoryOrderField exposing (RepositoryOrderField(..))
import Github.Interface.Actor exposing (avatarUrl)
import Github.Object
import Github.Object.ContributionsCollection
import Github.Object.FollowerConnection
import Github.Object.IssueConnection
import Github.Object.PullRequestConnection
import Github.Object.Repository
import Github.Object.RepositoryConnection
import Github.Object.RepositoryEdge
import Github.Object.StargazerConnection
import Github.Object.User as User
import Github.Query as Query
import Github.Scalar
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (..)
import Maybe.Extra exposing (combine)
import RemoteData exposing (..)


followers : SelectionSet Int Github.Object.User
followers =
    User.followers identity Github.Object.FollowerConnection.totalCount


pullRequests : SelectionSet Int Github.Object.User
pullRequests =
    User.pullRequests identity Github.Object.PullRequestConnection.totalCount


issues : SelectionSet Int Github.Object.User
issues =
    User.issues identity Github.Object.IssueConnection.totalCount


repositoriesContributedTo : SelectionSet Int Github.Object.User
repositoriesContributedTo =
    User.repositoriesContributedTo
        (\optionals ->
            { optionals
                | contributionTypes = Present [ Just Commit, Just Issue, Just PullRequest, Just Repository ]
            }
        )
        Github.Object.RepositoryConnection.totalCount


contributions : SelectionSet Int Github.Object.User
contributions =
    User.contributionsCollection identity Github.Object.ContributionsCollection.totalCommitContributions


repositories : SelectionSet RepositoryStat Github.Object.User
repositories =
    User.repositories
        (\optionals ->
            { optionals
                | first = Present 100
                , orderBy = Present { field = Stargazers, direction = Desc }
                , ownerAffiliations = Present [ Just Owner ]
            }
        )
        (SelectionSet.map2 RepositoryStat
            Github.Object.RepositoryConnection.totalCount
            stargazers
        )


stargazers : SelectionSet (List (Maybe Int)) Github.Object.RepositoryConnection
stargazers =
    Github.Object.RepositoryConnection.edges
        (Github.Object.RepositoryEdge.node
            (Github.Object.Repository.stargazers identity Github.Object.StargazerConnection.totalCount)
        )
        |> SelectionSet.nonNullOrFail
        |> SelectionSet.nonNullElementsOrFail


query : String -> SelectionSet Response RootQuery
query login =
    Query.user { login = login } <|
        SelectionSet.map8 GithubUser
            User.name
            (User.avatarUrl
                (\optionals ->
                    { optionals
                        | size = Present avatarSize
                    }
                )
            )
            followers
            pullRequests
            issues
            repositoriesContributedTo
            contributions
            repositories


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


type alias RepositoryStat =
    { owned : Int
    , stargazers : List (Maybe Int)
    }


type alias GithubUser =
    { name : Maybe String
    , avatarUrl : Github.Scalar.Uri
    , followers : Int
    , pullRequests : Int
    , issues : Int
    , contributedTo : Int
    , commits : Int
    , repositories : RepositoryStat
    }


init : String -> ( Model, Cmd Msg )
init flags =
    ( { apiToken = notBlank flags
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
            ( { model | apiToken = notBlank token }
            , saveToken token
            )

        EnteredLogin username ->
            ( { model | login = notBlank username }, Cmd.none )

        RequestUser ->
            let
                request : Cmd Msg
                request =
                    case combine [ model.apiToken, model.login ] of
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
        , label =
            Input.labelAbove
                [ Font.color (Element.rgb255 255 255 255)
                ]
                (Element.text "Github API Token")
        }
    , Input.text
        []
        { onChange = EnteredLogin
        , text = Maybe.withDefault "" login
        , placeholder = Input.placeholder [] (Element.text "Enter login") |> Just
        , label =
            Input.labelAbove
                [ Font.color (Element.rgb255 255 255 255)
                ]
                (Element.text "Github Login")
        }
    , [ apiToken, login ]
        |> combine
        |> Maybe.map
            (\_ ->
                Input.button
                    [ Element.centerX
                    , Element.width Element.fill
                    , Element.paddingXY 16 8
                    , Background.color (Element.rgb 255 255 255)
                    , Border.width 1
                    , Border.rounded 4
                    , Border.color (Element.rgb 0 0 0)
                    , Font.center
                    ]
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
        , Element.text <| "Repositories Owned: " ++ String.fromInt user.repositories.owned
        , Element.text <| "Repositories Contributed To: " ++ String.fromInt user.contributedTo
        , Element.text <| "Commits: " ++ String.fromInt user.commits
        , Element.text <| "Pull Requets: " ++ String.fromInt user.pullRequests
        , Element.text <| "Issues: " ++ String.fromInt user.issues
        , Element.text <| "Followers: " ++ String.fromInt user.followers
        , Element.text <|
            "Stargazers: "
                ++ String.fromInt (sumMaybeInt user.repositories.stargazers)
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
        Element.row
            [ Element.height Element.fill
            , Element.width Element.fill
            ]
            [ Element.column
                [ Element.height Element.fill
                , Element.width Element.fill
                , Background.color (Element.rgb255 94 139 222)
                ]
                [ Element.row
                    [ Element.centerX
                    , Element.centerY
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
                ]
            , Element.column
                [ Element.height Element.fill
                , Element.width (Element.fillPortion 2)
                ]
                [ Element.row
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
    200


notBlank : String -> Maybe String
notBlank str =
    if String.isEmpty str then
        Nothing

    else
        Just str


sumMaybeInt : List (Maybe Int) -> Int
sumMaybeInt list =
    List.foldl (+)
        0
        (list |> List.map (Maybe.withDefault 0))
