-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.Cvss exposing (..)

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


{-| The CVSS score associated with this advisory
-}
score : SelectionSet Float Github.Object.Cvss
score =
    Object.selectionForField "Float" "score" [] Decode.float


{-| The CVSS vector string associated with this advisory
-}
vectorString : SelectionSet (Maybe String) Github.Object.Cvss
vectorString =
    Object.selectionForField "(Maybe String)" "vectorString" [] (Decode.string |> Decode.nullable)
