-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Interface.ProfileOwner exposing (..)

import Github.Enum.PinnableItemType
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
    { onOrganization : SelectionSet decodesTo Github.Object.Organization
    , onUser : SelectionSet decodesTo Github.Object.User
    }


{-| Build an exhaustive selection of type-specific fragments.
-}
fragments :
    Fragments decodesTo
    -> SelectionSet decodesTo Github.Interface.ProfileOwner
fragments selections____ =
    Object.exhaustiveFragmentSelection
        [ Object.buildFragment "Organization" selections____.onOrganization
        , Object.buildFragment "User" selections____.onUser
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    { onOrganization = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onUser = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }


type alias AnyPinnableItemsOptionalArguments =
    { type_ : OptionalArgument Github.Enum.PinnableItemType.PinnableItemType }


{-| Determine if this repository owner has any items that can be pinned to their profile.

  - type\_ - Filter to only a particular kind of pinnable item.

-}
anyPinnableItems :
    (AnyPinnableItemsOptionalArguments -> AnyPinnableItemsOptionalArguments)
    -> SelectionSet Bool Github.Interface.ProfileOwner
anyPinnableItems fillInOptionals____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { type_ = Absent }

        optionalArgs____ =
            [ Argument.optional "type" filledInOptionals____.type_ (Encode.enum Github.Enum.PinnableItemType.toString) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForField "Bool" "anyPinnableItems" optionalArgs____ Decode.bool


{-| The public profile email.
-}
email : SelectionSet (Maybe String) Github.Interface.ProfileOwner
email =
    Object.selectionForField "(Maybe String)" "email" [] (Decode.string |> Decode.nullable)


id : SelectionSet Github.ScalarCodecs.Id Github.Interface.ProfileOwner
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Showcases a selection of repositories and gists that the profile owner has either curated or that have been selected automatically based on popularity.
-}
itemShowcase :
    SelectionSet decodesTo Github.Object.ProfileItemShowcase
    -> SelectionSet decodesTo Github.Interface.ProfileOwner
itemShowcase object____ =
    Object.selectionForCompositeField "itemShowcase" [] object____ Basics.identity


{-| The public profile location.
-}
location : SelectionSet (Maybe String) Github.Interface.ProfileOwner
location =
    Object.selectionForField "(Maybe String)" "location" [] (Decode.string |> Decode.nullable)


{-| The username used to login.
-}
login : SelectionSet String Github.Interface.ProfileOwner
login =
    Object.selectionForField "String" "login" [] Decode.string


{-| The public profile name.
-}
name : SelectionSet (Maybe String) Github.Interface.ProfileOwner
name =
    Object.selectionForField "(Maybe String)" "name" [] (Decode.string |> Decode.nullable)


type alias PinnableItemsOptionalArguments =
    { types : OptionalArgument (List Github.Enum.PinnableItemType.PinnableItemType)
    , after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list of repositories and gists this profile owner can pin to their profile.

  - types - Filter the types of pinnable items that are returned.
  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
pinnableItems :
    (PinnableItemsOptionalArguments -> PinnableItemsOptionalArguments)
    -> SelectionSet decodesTo Github.Object.PinnableItemConnection
    -> SelectionSet decodesTo Github.Interface.ProfileOwner
pinnableItems fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { types = Absent, after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs____ =
            [ Argument.optional "types" filledInOptionals____.types (Encode.enum Github.Enum.PinnableItemType.toString |> Encode.list), Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "last" filledInOptionals____.last Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "pinnableItems" optionalArgs____ object____ Basics.identity


type alias PinnedItemsOptionalArguments =
    { types : OptionalArgument (List Github.Enum.PinnableItemType.PinnableItemType)
    , after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list of repositories and gists this profile owner has pinned to their profile

  - types - Filter the types of pinned items that are returned.
  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
pinnedItems :
    (PinnedItemsOptionalArguments -> PinnedItemsOptionalArguments)
    -> SelectionSet decodesTo Github.Object.PinnableItemConnection
    -> SelectionSet decodesTo Github.Interface.ProfileOwner
pinnedItems fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { types = Absent, after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs____ =
            [ Argument.optional "types" filledInOptionals____.types (Encode.enum Github.Enum.PinnableItemType.toString |> Encode.list), Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "last" filledInOptionals____.last Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "pinnedItems" optionalArgs____ object____ Basics.identity


{-| Returns how many more items this profile owner can pin to their profile.
-}
pinnedItemsRemaining : SelectionSet Int Github.Interface.ProfileOwner
pinnedItemsRemaining =
    Object.selectionForField "Int" "pinnedItemsRemaining" [] Decode.int


{-| Can the viewer pin repositories and gists to the profile?
-}
viewerCanChangePinnedItems : SelectionSet Bool Github.Interface.ProfileOwner
viewerCanChangePinnedItems =
    Object.selectionForField "Bool" "viewerCanChangePinnedItems" [] Decode.bool


{-| The public profile website URL.
-}
websiteUrl : SelectionSet (Maybe Github.ScalarCodecs.Uri) Github.Interface.ProfileOwner
websiteUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "websiteUrl" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)
