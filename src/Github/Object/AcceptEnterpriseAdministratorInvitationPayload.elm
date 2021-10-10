-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.AcceptEnterpriseAdministratorInvitationPayload exposing (..)

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
clientMutationId : SelectionSet (Maybe String) Github.Object.AcceptEnterpriseAdministratorInvitationPayload
clientMutationId =
    Object.selectionForField "(Maybe String)" "clientMutationId" [] (Decode.string |> Decode.nullable)


{-| The invitation that was accepted.
-}
invitation :
    SelectionSet decodesTo Github.Object.EnterpriseAdministratorInvitation
    -> SelectionSet (Maybe decodesTo) Github.Object.AcceptEnterpriseAdministratorInvitationPayload
invitation object____ =
    Object.selectionForCompositeField "invitation" [] object____ (Basics.identity >> Decode.nullable)


{-| A message confirming the result of accepting an administrator invitation.
-}
message : SelectionSet (Maybe String) Github.Object.AcceptEnterpriseAdministratorInvitationPayload
message =
    Object.selectionForField "(Maybe String)" "message" [] (Decode.string |> Decode.nullable)
