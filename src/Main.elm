port module Main exposing (..)

import Browser
import Element
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
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (..)
import Maybe.Extra exposing (combine, or)
import Rank exposing (Rank(..), rank)
import RemoteData exposing (..)
import Style


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
        (SelectionSet.succeed GithubUser
            |> with User.name
            |> with User.login
            |> with
                (User.avatarUrl
                    (\optionals ->
                        { optionals
                            | size = Present Style.avatarSize
                        }
                    )
                )
            |> with followers
            |> with pullRequests
            |> with issues
            |> with repositoriesContributedTo
            |> with contributions
            |> with repositories
        )


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
    , login : String
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


type Field
    = ApiToken
    | Login


type Msg
    = Input Field String
    | RequestUser
    | GotResponse (RemoteData (Graphql.Http.Error Response) Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input field value ->
            case field of
                ApiToken ->
                    ( { model | apiToken = notBlank value }
                    , saveToken value
                    )

                Login ->
                    ( { model | login = notBlank value }, Cmd.none )

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
    [ Element.column []
        [ Input.text
            []
            { onChange = Input ApiToken
            , text = Maybe.withDefault "" apiToken
            , placeholder = Input.placeholder [] (Element.text "Enter API Token") |> Just
            , label =
                Input.labelAbove
                    [ Font.color (Element.rgb255 255 255 255)
                    ]
                    (Element.text "Github API Token")
            }
        , Element.newTabLink
            [ Element.paddingXY 0 4
            , Font.size 15
            , Font.extraLight
            , Font.underline
            , Font.color (Element.rgb255 255 255 255)
            ]
            { url = "https://github.com/settings/tokens"
            , label = Element.text "Create a token"
            }
        ]
    , Input.text
        []
        { onChange = Input Login
        , text = Maybe.withDefault "" login
        , placeholder = Input.placeholder [] (Element.text "Enter login") |> Just
        , label =
            Input.labelAbove
                [ Font.color Style.white
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
                    , Background.color Style.white
                    , Border.width 1
                    , Border.rounded 4
                    , Border.color Style.black
                    , Font.center
                    ]
                    { onPress = Just RequestUser
                    , label = Element.text "Search"
                    }
            )
        |> Maybe.withDefault Element.none
    ]


viewAvatar : Github.Scalar.Uri -> Element.Element msg
viewAvatar (Github.Scalar.Uri avatarUrl) =
    Element.image
        [ Element.centerX
        , Element.height <| Element.px Style.avatarSize
        , Element.width <| Element.px Style.avatarSize
        , Element.clip
        , Border.rounded <| (Style.avatarSize // 2)
        ]
        { src = avatarUrl
        , description = "Github avatar"
        }


statRow : String -> String -> Element.Element Msg
statRow label stat =
    Element.row
        [ Element.width Element.fill ]
        [ Element.el
            [ Element.width (Element.fillPortion 6)
            ]
            (Element.text label)
        , Element.el
            [ Element.width (Element.fillPortion 1)
            , Font.alignLeft
            ]
            (Element.text stat)
        ]


viewResult : GithubUser -> Element.Element Msg
viewResult user =
    let
        rank : Rank.RankResult
        rank =
            Rank.rank
                { totalRepos = user.repositories.owned
                , totalCommits = user.commits
                , contributions = user.contributedTo
                , followers = user.followers
                , pullRequests = user.pullRequests
                , issues = user.issues
                , stargazers = sumMaybeInt user.repositories.stargazers
                }
    in
    Element.column
        [ Element.spacing 8, Element.width (Element.px 450) ]
        [ viewAvatar user.avatarUrl
        , Element.row [ Element.spacing 8 ] [ Rank.viewRank rank.rank rank.score ]
        , Element.row
            [ Element.alignLeft
            , Element.width Element.fill
            , Font.semiBold
            ]
            [ user.name
                |> or (Just user.login)
                |> Maybe.map (\name -> Element.text (name ++ "'s Github Stats"))
                |> Maybe.withDefault Element.none
            ]
        , statRow "Total Stars Earned:" <|
            String.fromInt <|
                sumMaybeInt user.repositories.stargazers
        , statRow "Total Commits:" <| String.fromInt user.commits
        , statRow "Total PRs:" <| String.fromInt user.pullRequests
        , statRow "Total Issues:" <| String.fromInt user.issues
        , statRow "Contributed to:" <| String.fromInt user.contributedTo
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
                , Background.color Style.blue
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
                            Element.none

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
