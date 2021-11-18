module Card exposing (..)

import Color
import Element
import Rank exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (text)
import TypedSvg.Types exposing (..)


viewRank : Rank -> Element.Element msg
viewRank rank =
    Element.html <|
        svg
            [ width (px 450)
            , height (px 100)
            , x (px 0)
            , y (px 0)
            ]
            [ g
                [ transform [ Translate 225 50 ]
                ]
                [ circle
                    [ cy (px 0)
                    , r (px 45)
                    , noFill
                    , stroke <| Paint (Color.rgb255 27 63 131)
                    , strokeWidth (px 1)
                    ]
                    []
                , circle
                    [ cy (px 0)
                    , r (px 45)
                    , noFill
                    , stroke <| Paint (Color.rgb255 172 200 229)
                    , strokeWidth (px 5)
                    ]
                    []
                , g []
                    [ text_
                        [ x (px 0)
                        , y (px 0)
                        , alignmentBaseline AlignmentCentral
                        , dominantBaseline DominantBaselineCentral
                        , textAnchor AnchorMiddle
                        , fontSize (px 40)
                        , fill <| Paint (Color.rgb255 27 63 131)
                        ]
                        [ text (Rank.toString rank) ]
                    ]
                ]
            ]
