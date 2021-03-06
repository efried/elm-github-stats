-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.CreateCommitOnBranchPayload exposing (..)

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


{-| A unique identifier for the client performing the mutation.
-}
clientMutationId : SelectionSet (Maybe String) Github.Object.CreateCommitOnBranchPayload
clientMutationId =
    Object.selectionForField "(Maybe String)" "clientMutationId" [] (Decode.string |> Decode.nullable)


{-| The new commit.
-}
commit :
    SelectionSet decodesTo Github.Object.Commit
    -> SelectionSet (Maybe decodesTo) Github.Object.CreateCommitOnBranchPayload
commit object____ =
    Object.selectionForCompositeField "commit" [] object____ (Basics.identity >> Decode.nullable)


{-| The ref which has been updated to point to the new commit.
-}
ref :
    SelectionSet decodesTo Github.Object.Ref
    -> SelectionSet (Maybe decodesTo) Github.Object.CreateCommitOnBranchPayload
ref object____ =
    Object.selectionForCompositeField "ref" [] object____ (Basics.identity >> Decode.nullable)
