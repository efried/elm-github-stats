-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Interface.RequirableByPullRequest exposing (..)

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
import Graphql.SelectionSet exposing (FragmentSelectionSet(..), SelectionSet(..))
import Json.Decode as Decode


type alias Fragments decodesTo =
    { onCheckRun : SelectionSet decodesTo Github.Object.CheckRun
    , onStatusContext : SelectionSet decodesTo Github.Object.StatusContext
    }


{-| Build an exhaustive selection of type-specific fragments.
-}
fragments :
    Fragments decodesTo
    -> SelectionSet decodesTo Github.Interface.RequirableByPullRequest
fragments selections____ =
    Object.exhaustiveFragmentSelection
        [ Object.buildFragment "CheckRun" selections____.onCheckRun
        , Object.buildFragment "StatusContext" selections____.onStatusContext
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    { onCheckRun = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onStatusContext = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }


type alias IsRequiredOptionalArguments =
    { pullRequestId : OptionalArgument Github.ScalarCodecs.Id
    , pullRequestNumber : OptionalArgument Int
    }


{-| Whether this is required to pass before merging for a specific pull request.

  - pullRequestId - The id of the pull request this is required for
  - pullRequestNumber - The number of the pull request this is required for

-}
isRequired :
    (IsRequiredOptionalArguments -> IsRequiredOptionalArguments)
    -> SelectionSet Bool Github.Interface.RequirableByPullRequest
isRequired fillInOptionals____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { pullRequestId = Absent, pullRequestNumber = Absent }

        optionalArgs____ =
            [ Argument.optional "pullRequestId" filledInOptionals____.pullRequestId (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapEncoder .codecId), Argument.optional "pullRequestNumber" filledInOptionals____.pullRequestNumber Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForField "Bool" "isRequired" optionalArgs____ Decode.bool
