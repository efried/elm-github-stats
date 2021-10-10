-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.OrgRemoveBillingManagerAuditEntry exposing (..)

import Github.Enum.OperationType
import Github.Enum.OrgRemoveBillingManagerAuditEntryReason
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


{-| The action name
-}
action : SelectionSet String Github.Object.OrgRemoveBillingManagerAuditEntry
action =
    Object.selectionForField "String" "action" [] Decode.string


{-| The user who initiated the action
-}
actor :
    SelectionSet decodesTo Github.Union.AuditEntryActor
    -> SelectionSet (Maybe decodesTo) Github.Object.OrgRemoveBillingManagerAuditEntry
actor object____ =
    Object.selectionForCompositeField "actor" [] object____ (Basics.identity >> Decode.nullable)


{-| The IP address of the actor
-}
actorIp : SelectionSet (Maybe String) Github.Object.OrgRemoveBillingManagerAuditEntry
actorIp =
    Object.selectionForField "(Maybe String)" "actorIp" [] (Decode.string |> Decode.nullable)


{-| A readable representation of the actor's location
-}
actorLocation :
    SelectionSet decodesTo Github.Object.ActorLocation
    -> SelectionSet (Maybe decodesTo) Github.Object.OrgRemoveBillingManagerAuditEntry
actorLocation object____ =
    Object.selectionForCompositeField "actorLocation" [] object____ (Basics.identity >> Decode.nullable)


{-| The username of the user who initiated the action
-}
actorLogin : SelectionSet (Maybe String) Github.Object.OrgRemoveBillingManagerAuditEntry
actorLogin =
    Object.selectionForField "(Maybe String)" "actorLogin" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for the actor.
-}
actorResourcePath : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.OrgRemoveBillingManagerAuditEntry
actorResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "actorResourcePath" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for the actor.
-}
actorUrl : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.OrgRemoveBillingManagerAuditEntry
actorUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "actorUrl" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The time the action was initiated
-}
createdAt : SelectionSet Github.ScalarCodecs.PreciseDateTime Github.Object.OrgRemoveBillingManagerAuditEntry
createdAt =
    Object.selectionForField "ScalarCodecs.PreciseDateTime" "createdAt" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecPreciseDateTime |> .decoder)


id : SelectionSet Github.ScalarCodecs.Id Github.Object.OrgRemoveBillingManagerAuditEntry
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The corresponding operation type for the action
-}
operationType : SelectionSet (Maybe Github.Enum.OperationType.OperationType) Github.Object.OrgRemoveBillingManagerAuditEntry
operationType =
    Object.selectionForField "(Maybe Enum.OperationType.OperationType)" "operationType" [] (Github.Enum.OperationType.decoder |> Decode.nullable)


{-| The Organization associated with the Audit Entry.
-}
organization :
    SelectionSet decodesTo Github.Object.Organization
    -> SelectionSet (Maybe decodesTo) Github.Object.OrgRemoveBillingManagerAuditEntry
organization object____ =
    Object.selectionForCompositeField "organization" [] object____ (Basics.identity >> Decode.nullable)


{-| The name of the Organization.
-}
organizationName : SelectionSet (Maybe String) Github.Object.OrgRemoveBillingManagerAuditEntry
organizationName =
    Object.selectionForField "(Maybe String)" "organizationName" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for the organization
-}
organizationResourcePath : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.OrgRemoveBillingManagerAuditEntry
organizationResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "organizationResourcePath" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for the organization
-}
organizationUrl : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.OrgRemoveBillingManagerAuditEntry
organizationUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "organizationUrl" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The reason for the billing manager being removed.
-}
reason : SelectionSet (Maybe Github.Enum.OrgRemoveBillingManagerAuditEntryReason.OrgRemoveBillingManagerAuditEntryReason) Github.Object.OrgRemoveBillingManagerAuditEntry
reason =
    Object.selectionForField "(Maybe Enum.OrgRemoveBillingManagerAuditEntryReason.OrgRemoveBillingManagerAuditEntryReason)" "reason" [] (Github.Enum.OrgRemoveBillingManagerAuditEntryReason.decoder |> Decode.nullable)


{-| The user affected by the action
-}
user :
    SelectionSet decodesTo Github.Object.User
    -> SelectionSet (Maybe decodesTo) Github.Object.OrgRemoveBillingManagerAuditEntry
user object____ =
    Object.selectionForCompositeField "user" [] object____ (Basics.identity >> Decode.nullable)


{-| For actions involving two users, the actor is the initiator and the user is the affected user.
-}
userLogin : SelectionSet (Maybe String) Github.Object.OrgRemoveBillingManagerAuditEntry
userLogin =
    Object.selectionForField "(Maybe String)" "userLogin" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for the user.
-}
userResourcePath : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.OrgRemoveBillingManagerAuditEntry
userResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "userResourcePath" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for the user.
-}
userUrl : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.OrgRemoveBillingManagerAuditEntry
userUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "userUrl" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)
