-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.Sponsorship exposing (..)

import Github.Enum.SponsorshipPrivacy
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
createdAt : SelectionSet Github.ScalarCodecs.DateTime Github.Object.Sponsorship
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


id : SelectionSet Github.ScalarCodecs.Id Github.Object.Sponsorship
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Whether this sponsorship represents a one-time payment versus a recurring sponsorship.
-}
isOneTimePayment : SelectionSet Bool Github.Object.Sponsorship
isOneTimePayment =
    Object.selectionForField "Bool" "isOneTimePayment" [] Decode.bool


{-| Check if the sponsor has chosen to receive sponsorship update emails sent from the sponsorable. Only returns a non-null value when the viewer has permission to know this.
-}
isSponsorOptedIntoEmail : SelectionSet (Maybe Bool) Github.Object.Sponsorship
isSponsorOptedIntoEmail =
    Object.selectionForField "(Maybe Bool)" "isSponsorOptedIntoEmail" [] (Decode.bool |> Decode.nullable)


{-| The entity that is being sponsored
-}
maintainer :
    SelectionSet decodesTo Github.Object.User
    -> SelectionSet decodesTo Github.Object.Sponsorship
maintainer object____ =
    Object.selectionForCompositeField "maintainer" [] object____ Basics.identity


{-| The privacy level for this sponsorship.
-}
privacyLevel : SelectionSet Github.Enum.SponsorshipPrivacy.SponsorshipPrivacy Github.Object.Sponsorship
privacyLevel =
    Object.selectionForField "Enum.SponsorshipPrivacy.SponsorshipPrivacy" "privacyLevel" [] Github.Enum.SponsorshipPrivacy.decoder


{-| The user that is sponsoring. Returns null if the sponsorship is private or if sponsor is not a user.
-}
sponsor :
    SelectionSet decodesTo Github.Object.User
    -> SelectionSet (Maybe decodesTo) Github.Object.Sponsorship
sponsor object____ =
    Object.selectionForCompositeField "sponsor" [] object____ (Basics.identity >> Decode.nullable)


{-| The user or organization that is sponsoring, if you have permission to view them.
-}
sponsorEntity :
    SelectionSet decodesTo Github.Union.Sponsor
    -> SelectionSet (Maybe decodesTo) Github.Object.Sponsorship
sponsorEntity object____ =
    Object.selectionForCompositeField "sponsorEntity" [] object____ (Basics.identity >> Decode.nullable)


{-| The entity that is being sponsored
-}
sponsorable :
    SelectionSet decodesTo Github.Interface.Sponsorable
    -> SelectionSet decodesTo Github.Object.Sponsorship
sponsorable object____ =
    Object.selectionForCompositeField "sponsorable" [] object____ Basics.identity


{-| The associated sponsorship tier
-}
tier :
    SelectionSet decodesTo Github.Object.SponsorsTier
    -> SelectionSet (Maybe decodesTo) Github.Object.Sponsorship
tier object____ =
    Object.selectionForCompositeField "tier" [] object____ (Basics.identity >> Decode.nullable)


{-| Identifies the date and time when the current tier was chosen for this sponsorship.
-}
tierSelectedAt : SelectionSet (Maybe Github.ScalarCodecs.DateTime) Github.Object.Sponsorship
tierSelectedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "tierSelectedAt" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)
