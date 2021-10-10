-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.RefUpdateRule exposing (..)

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


{-| Can this branch be deleted.
-}
allowsDeletions : SelectionSet Bool Github.Object.RefUpdateRule
allowsDeletions =
    Object.selectionForField "Bool" "allowsDeletions" [] Decode.bool


{-| Are force pushes allowed on this branch.
-}
allowsForcePushes : SelectionSet Bool Github.Object.RefUpdateRule
allowsForcePushes =
    Object.selectionForField "Bool" "allowsForcePushes" [] Decode.bool


{-| Identifies the protection rule pattern.
-}
pattern : SelectionSet String Github.Object.RefUpdateRule
pattern =
    Object.selectionForField "String" "pattern" [] Decode.string


{-| Number of approving reviews required to update matching branches.
-}
requiredApprovingReviewCount : SelectionSet (Maybe Int) Github.Object.RefUpdateRule
requiredApprovingReviewCount =
    Object.selectionForField "(Maybe Int)" "requiredApprovingReviewCount" [] (Decode.int |> Decode.nullable)


{-| List of required status check contexts that must pass for commits to be accepted to matching branches.
-}
requiredStatusCheckContexts : SelectionSet (Maybe (List (Maybe String))) Github.Object.RefUpdateRule
requiredStatusCheckContexts =
    Object.selectionForField "(Maybe (List (Maybe String)))" "requiredStatusCheckContexts" [] (Decode.string |> Decode.nullable |> Decode.list |> Decode.nullable)


{-| Are reviews from code owners required to update matching branches.
-}
requiresCodeOwnerReviews : SelectionSet Bool Github.Object.RefUpdateRule
requiresCodeOwnerReviews =
    Object.selectionForField "Bool" "requiresCodeOwnerReviews" [] Decode.bool


{-| Are conversations required to be resolved before merging.
-}
requiresConversationResolution : SelectionSet Bool Github.Object.RefUpdateRule
requiresConversationResolution =
    Object.selectionForField "Bool" "requiresConversationResolution" [] Decode.bool


{-| Are merge commits prohibited from being pushed to this branch.
-}
requiresLinearHistory : SelectionSet Bool Github.Object.RefUpdateRule
requiresLinearHistory =
    Object.selectionForField "Bool" "requiresLinearHistory" [] Decode.bool


{-| Are commits required to be signed.
-}
requiresSignatures : SelectionSet Bool Github.Object.RefUpdateRule
requiresSignatures =
    Object.selectionForField "Bool" "requiresSignatures" [] Decode.bool


{-| Is the viewer allowed to dismiss reviews.
-}
viewerAllowedToDismissReviews : SelectionSet Bool Github.Object.RefUpdateRule
viewerAllowedToDismissReviews =
    Object.selectionForField "Bool" "viewerAllowedToDismissReviews" [] Decode.bool


{-| Can the viewer push to the branch
-}
viewerCanPush : SelectionSet Bool Github.Object.RefUpdateRule
viewerCanPush =
    Object.selectionForField "Bool" "viewerCanPush" [] Decode.bool
