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
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Types


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


type Symbol
    = PRs
    | Commits
    | Stars
    | Issues
    | Contribs


viewSymbol : Symbol -> Element.Element Msg
viewSymbol symbol =
    let
        pathD : String
        pathD =
            if symbol == PRs then
                "M7.177 3.073L9.573.677A.25.25 0 0110 .854v4.792a.25.25 0 01-.427.177L7.177 3.427a.25.25 0 010-.354zM3.75 2.5a.75.75 0 100 1.5.75.75 0 000-1.5zm-2.25.75a2.25 2.25 0 113 2.122v5.256a2.251 2.251 0 11-1.5 0V5.372A2.25 2.25 0 011.5 3.25zM11 2.5h-1V4h1a1 1 0 011 1v5.628a2.251 2.251 0 101.5 0V5A2.5 2.5 0 0011 2.5zm1 10.25a.75.75 0 111.5 0 .75.75 0 01-1.5 0zM3.75 12a.75.75 0 100 1.5.75.75 0 000-1.5z"

            else if symbol == Commits then
                "M1.643 3.143L.427 1.927A.25.25 0 000 2.104V5.75c0 .138.112.25.25.25h3.646a.25.25 0 00.177-.427L2.715 4.215a6.5 6.5 0 11-1.18 4.458.75.75 0 10-1.493.154 8.001 8.001 0 101.6-5.684zM7.75 4a.75.75 0 01.75.75v2.992l2.028.812a.75.75 0 01-.557 1.392l-2.5-1A.75.75 0 017 8.25v-3.5A.75.75 0 017.75 4z"

            else if symbol == Stars then
                "M8 .25a.75.75 0 01.673.418l1.882 3.815 4.21.612a.75.75 0 01.416 1.279l-3.046 2.97.719 4.192a.75.75 0 01-1.088.791L8 12.347l-3.766 1.98a.75.75 0 01-1.088-.79l.72-4.194L.818 6.374a.75.75 0 01.416-1.28l4.21-.611L7.327.668A.75.75 0 018 .25zm0 2.445L6.615 5.5a.75.75 0 01-.564.41l-3.097.45 2.24 2.184a.75.75 0 01.216.664l-.528 3.084 2.769-1.456a.75.75 0 01.698 0l2.77 1.456-.53-3.084a.75.75 0 01.216-.664l2.24-2.183-3.096-.45a.75.75 0 01-.564-.41L8 2.694v.001z"

            else if symbol == Issues then
                "M8 1.5a6.5 6.5 0 100 13 6.5 6.5 0 000-13zM0 8a8 8 0 1116 0A8 8 0 010 8zm9 3a1 1 0 11-2 0 1 1 0 012 0zm-.25-6.25a.75.75 0 00-1.5 0v3.5a.75.75 0 001.5 0v-3.5z"

            else
                "M2 2.5A2.5 2.5 0 014.5 0h8.75a.75.75 0 01.75.75v12.5a.75.75 0 01-.75.75h-2.5a.75.75 0 110-1.5h1.75v-2h-8a1 1 0 00-.714 1.7.75.75 0 01-1.072 1.05A2.495 2.495 0 012 11.5v-9zm10.5-1V9h-8c-.356 0-.694.074-1 .208V2.5a1 1 0 011-1h8zM5 12.25v3.25a.25.25 0 00.4.2l1.45-1.087a.25.25 0 01.3 0L8.6 15.7a.25.25 0 00.4-.2v-3.25a.25.25 0 00-.25-.25h-3.5a.25.25 0 00-.25.25z"
    in
    Element.html <|
        TypedSvg.svg
            [ TypedSvg.Attributes.width (TypedSvg.Types.px 20)
            , TypedSvg.Attributes.height (TypedSvg.Types.px 20)
            ]
            [ TypedSvg.path
                [ TypedSvg.Attributes.fillRule TypedSvg.Types.FillRuleEvenOdd
                , TypedSvg.Attributes.d pathD
                ]
                []
            ]


statRow : Symbol -> String -> String -> Element.Element Msg
statRow symbol label stat =
    Element.row
        [ Element.width Element.fill ]
        [ Element.row
            [ Element.centerY
            , Element.width (Element.fillPortion 6)
            ]
            [ viewSymbol symbol
            , Element.el
                [ Element.paddingXY 8 0 ]
                (Element.text label)
            ]
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
        , statRow Stars "Total Stars Earned:" <|
            String.fromInt <|
                sumMaybeInt user.repositories.stargazers
        , statRow Commits "Total Commits:" <| String.fromInt user.commits
        , statRow PRs "Total PRs:" <| String.fromInt user.pullRequests
        , statRow Issues "Total Issues:" <| String.fromInt user.issues
        , statRow Contribs "Contributed to:" <| String.fromInt user.contributedTo
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
