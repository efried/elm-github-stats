-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.DeploymentProtectionRule exposing (..)

import Github.Enum.DeploymentProtectionRuleType
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


{-| Identifies the primary key from the database.
-}
databaseId : SelectionSet (Maybe Int) Github.Object.DeploymentProtectionRule
databaseId =
    Object.selectionForField "(Maybe Int)" "databaseId" [] (Decode.int |> Decode.nullable)


type alias ReviewersOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| The teams or users that can review the deployment

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
reviewers :
    (ReviewersOptionalArguments -> ReviewersOptionalArguments)
    -> SelectionSet decodesTo Github.Object.DeploymentReviewerConnection
    -> SelectionSet decodesTo Github.Object.DeploymentProtectionRule
reviewers fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs____ =
            [ Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "last" filledInOptionals____.last Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "reviewers" optionalArgs____ object____ Basics.identity


{-| The timeout in minutes for this protection rule.
-}
timeout : SelectionSet Int Github.Object.DeploymentProtectionRule
timeout =
    Object.selectionForField "Int" "timeout" [] Decode.int


{-| The type of protection rule.
-}
type_ : SelectionSet Github.Enum.DeploymentProtectionRuleType.DeploymentProtectionRuleType Github.Object.DeploymentProtectionRule
type_ =
    Object.selectionForField "Enum.DeploymentProtectionRuleType.DeploymentProtectionRuleType" "type" [] Github.Enum.DeploymentProtectionRuleType.decoder
