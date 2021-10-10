-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.PrivateRepositoryForkingDisableAuditEntry exposing (..)

import Github.Enum.OperationType
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
action : SelectionSet String Github.Object.PrivateRepositoryForkingDisableAuditEntry
action =
    Object.selectionForField "String" "action" [] Decode.string


{-| The user who initiated the action
-}
actor :
    SelectionSet decodesTo Github.Union.AuditEntryActor
    -> SelectionSet (Maybe decodesTo) Github.Object.PrivateRepositoryForkingDisableAuditEntry
actor object____ =
    Object.selectionForCompositeField "actor" [] object____ (Basics.identity >> Decode.nullable)


{-| The IP address of the actor
-}
actorIp : SelectionSet (Maybe String) Github.Object.PrivateRepositoryForkingDisableAuditEntry
actorIp =
    Object.selectionForField "(Maybe String)" "actorIp" [] (Decode.string |> Decode.nullable)


{-| A readable representation of the actor's location
-}
actorLocation :
    SelectionSet decodesTo Github.Object.ActorLocation
    -> SelectionSet (Maybe decodesTo) Github.Object.PrivateRepositoryForkingDisableAuditEntry
actorLocation object____ =
    Object.selectionForCompositeField "actorLocation" [] object____ (Basics.identity >> Decode.nullable)


{-| The username of the user who initiated the action
-}
actorLogin : SelectionSet (Maybe String) Github.Object.PrivateRepositoryForkingDisableAuditEntry
actorLogin =
    Object.selectionForField "(Maybe String)" "actorLogin" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for the actor.
-}
actorResourcePath : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.PrivateRepositoryForkingDisableAuditEntry
actorResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "actorResourcePath" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for the actor.
-}
actorUrl : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.PrivateRepositoryForkingDisableAuditEntry
actorUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "actorUrl" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The time the action was initiated
-}
createdAt : SelectionSet Github.ScalarCodecs.PreciseDateTime Github.Object.PrivateRepositoryForkingDisableAuditEntry
createdAt =
    Object.selectionForField "ScalarCodecs.PreciseDateTime" "createdAt" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecPreciseDateTime |> .decoder)


{-| The HTTP path for this enterprise.
-}
enterpriseResourcePath : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.PrivateRepositoryForkingDisableAuditEntry
enterpriseResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "enterpriseResourcePath" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The slug of the enterprise.
-}
enterpriseSlug : SelectionSet (Maybe String) Github.Object.PrivateRepositoryForkingDisableAuditEntry
enterpriseSlug =
    Object.selectionForField "(Maybe String)" "enterpriseSlug" [] (Decode.string |> Decode.nullable)


{-| The HTTP URL for this enterprise.
-}
enterpriseUrl : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.PrivateRepositoryForkingDisableAuditEntry
enterpriseUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "enterpriseUrl" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


id : SelectionSet Github.ScalarCodecs.Id Github.Object.PrivateRepositoryForkingDisableAuditEntry
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The corresponding operation type for the action
-}
operationType : SelectionSet (Maybe Github.Enum.OperationType.OperationType) Github.Object.PrivateRepositoryForkingDisableAuditEntry
operationType =
    Object.selectionForField "(Maybe Enum.OperationType.OperationType)" "operationType" [] (Github.Enum.OperationType.decoder |> Decode.nullable)


{-| The Organization associated with the Audit Entry.
-}
organization :
    SelectionSet decodesTo Github.Object.Organization
    -> SelectionSet (Maybe decodesTo) Github.Object.PrivateRepositoryForkingDisableAuditEntry
organization object____ =
    Object.selectionForCompositeField "organization" [] object____ (Basics.identity >> Decode.nullable)


{-| The name of the Organization.
-}
organizationName : SelectionSet (Maybe String) Github.Object.PrivateRepositoryForkingDisableAuditEntry
organizationName =
    Object.selectionForField "(Maybe String)" "organizationName" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for the organization
-}
organizationResourcePath : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.PrivateRepositoryForkingDisableAuditEntry
organizationResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "organizationResourcePath" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for the organization
-}
organizationUrl : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.PrivateRepositoryForkingDisableAuditEntry
organizationUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "organizationUrl" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The repository associated with the action
-}
repository :
    SelectionSet decodesTo Github.Object.Repository
    -> SelectionSet (Maybe decodesTo) Github.Object.PrivateRepositoryForkingDisableAuditEntry
repository object____ =
    Object.selectionForCompositeField "repository" [] object____ (Basics.identity >> Decode.nullable)


{-| The name of the repository
-}
repositoryName : SelectionSet (Maybe String) Github.Object.PrivateRepositoryForkingDisableAuditEntry
repositoryName =
    Object.selectionForField "(Maybe String)" "repositoryName" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for the repository
-}
repositoryResourcePath : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.PrivateRepositoryForkingDisableAuditEntry
repositoryResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "repositoryResourcePath" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for the repository
-}
repositoryUrl : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.PrivateRepositoryForkingDisableAuditEntry
repositoryUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "repositoryUrl" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The user affected by the action
-}
user :
    SelectionSet decodesTo Github.Object.User
    -> SelectionSet (Maybe decodesTo) Github.Object.PrivateRepositoryForkingDisableAuditEntry
user object____ =
    Object.selectionForCompositeField "user" [] object____ (Basics.identity >> Decode.nullable)


{-| For actions involving two users, the actor is the initiator and the user is the affected user.
-}
userLogin : SelectionSet (Maybe String) Github.Object.PrivateRepositoryForkingDisableAuditEntry
userLogin =
    Object.selectionForField "(Maybe String)" "userLogin" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for the user.
-}
userResourcePath : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.PrivateRepositoryForkingDisableAuditEntry
userResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "userResourcePath" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for the user.
-}
userUrl : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Object.PrivateRepositoryForkingDisableAuditEntry
userUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "userUrl" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)
