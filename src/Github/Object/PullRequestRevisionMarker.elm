-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.PullRequestRevisionMarker exposing (..)

import Github.InputObject
import Github.Interface
import Github.Object
import Github.Scalar
import Github.ScalarCodecs
import Github.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Github.ScalarCodecs.DateTime Github.Object.PullRequestRevisionMarker
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The last commit the viewer has seen.
-}
lastSeenCommit :
    SelectionSet decodesTo Github.Object.Commit
    -> SelectionSet decodesTo Github.Object.PullRequestRevisionMarker
lastSeenCommit object____ =
    Object.selectionForCompositeField "lastSeenCommit" [] object____ Basics.identity


{-| The pull request to which the marker belongs.
-}
pullRequest :
    SelectionSet decodesTo Github.Object.PullRequest
    -> SelectionSet decodesTo Github.Object.PullRequestRevisionMarker
pullRequest object____ =
    Object.selectionForCompositeField "pullRequest" [] object____ Basics.identity
