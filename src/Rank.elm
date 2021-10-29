module Rank exposing (Rank, RankResult, rank, toString)

-- Normal CDF function from https://www.math.ucla.edu/~tom/distributions/normal.html?


normalCdf : Float -> Float -> Float -> Float
normalCdf mean sigma to =
    let
        x : Float
        x =
            (to - mean) / sigma

        t : Float
        t =
            1 / (1 + 0.2315419 * abs x)

        d : Float
        d =
            0.3989423 * e ^ (-x * x / 2)

        prob : Float
        prob =
            d
                * t
                * (0.3193815
                    + t
                    * (-0.3565638
                        + t
                        * (1.781478
                            + t
                            * (-1.821256 + t * 1.330274)
                          )
                      )
                  )
    in
    if x > 0 then
        1 - prob

    else
        prob


type alias RankArguments =
    { totalRepos : Int
    , totalCommits : Int
    , contributions : Int
    , followers : Int
    , pullRequests : Int
    , issues : Int
    , stargazers : Int
    }


type alias RankResult =
    { score : Float
    , rank : Rank
    }


type Rank
    = C
    | BPlus
    | APlus
    | APlusPlus
    | S
    | SPlus


rank : RankArguments -> RankResult
rank stats =
    let
        allOffsets : Float
        allOffsets =
            List.sum [ contribsOffset, issuesOffset, starsOffset, prsOffset, followersOffset, reposOffset ]

        totalValues : Float
        totalValues =
            List.sum [ rankSValue, rankA2Value, rankA3Value, rankBValue ]

        score : Float
        score =
            ((toFloat stats.totalCommits * commitsOffset)
                + (toFloat stats.contributions * contribsOffset)
                + (toFloat stats.issues * issuesOffset)
                + (toFloat stats.stargazers * starsOffset)
                + (toFloat stats.pullRequests * prsOffset)
                + (toFloat stats.followers * followersOffset)
                + (toFloat stats.totalRepos * reposOffset)
            )
                / 100

        normalizedScore : Float
        normalizedScore =
            normalCdf score totalValues allOffsets
                |> (*) 100

        letterRank : Rank
        letterRank =
            if normalizedScore < rankSValue then
                SPlus

            else if normalizedScore >= rankSValue && normalizedScore < rankDoubleAValue then
                S

            else if normalizedScore >= rankDoubleAValue && normalizedScore < rankA2Value then
                APlusPlus

            else if normalizedScore >= rankA2Value && normalizedScore < rankA3Value then
                APlus

            else if normalizedScore >= rankA3Value && normalizedScore < rankBValue then
                BPlus

            else
                C
    in
    RankResult normalizedScore letterRank


toString : Rank -> String
toString enum =
    case enum of
        C ->
            "C"

        BPlus ->
            "B+"

        APlus ->
            "A+"

        APlusPlus ->
            "A++"

        S ->
            "S"

        SPlus ->
            "S+"



-- OFFSETS


commitsOffset : Float
commitsOffset =
    1.65


contribsOffset : Float
contribsOffset =
    1.65


issuesOffset : Float
issuesOffset =
    1


starsOffset : Float
starsOffset =
    0.75


prsOffset : Float
prsOffset =
    0.5


followersOffset : Float
followersOffset =
    0.45


reposOffset : Float
reposOffset =
    1



-- VALUES


rankSValue : Float
rankSValue =
    1


rankDoubleAValue : Float
rankDoubleAValue =
    25


rankA2Value : Float
rankA2Value =
    45


rankA3Value : Float
rankA3Value =
    60


rankBValue : Float
rankBValue =
    100
