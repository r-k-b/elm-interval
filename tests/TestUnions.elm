module TestUnions exposing (suite)

import Expect exposing (Expectation)
import Test exposing (describe, test, Test)
import Interval
    exposing
        ( empty
        , excludes
        , includes
        , interval
        , intervalToString
        )
import Union
    exposing
        ( fromInterval
        , union
        , unionOfIntervals
        , unionToString
        )


a =
    -- [1, 2)
    interval (includes 1) (excludes 2)


b =
    -- [3, 4]
    interval (includes 3) (includes 4)


c =
    -- [2, 3]
    interval (includes 2) (includes 3)


d =
    -- (2, 5]
    interval (excludes 2) (includes 5)


suite : Test
suite =
    describe "unions"
        [ describe "constructors"
            [ test "from an interval" <|
                \_ ->
                    fromInterval a
                        |> unionToString
                        |> Expect.equal "{ [1, 2) }"
            , test "from an empty interval" <|
                \_ ->
                    fromInterval empty
                        |> unionToString
                        |> Expect.equal "{}"
            ]
        , describe "unions of intervals"
            [ test "separate intervals" <|
                -- [1, 2) ∪ [3, 4] = { [1, 2), [3, 4] }
                \_ ->
                    unionOfIntervals a b
                        |> unionToString
                        |> Expect.equal "{ [1, 2), [3, 4] }"
            , test "adjoint intervals" <|
                -- [1, 2) ∪ [2, 3] = { [1, 3] }
                \_ ->
                    unionOfIntervals a c
                        |> unionToString
                        |> Expect.equal "{ [1, 3] }"
            , test "not-quite-adjoint intervals" <|
                -- [1, 2) ∪ (2, 5] = { [1, 2), (2, 5] }
                \_ ->
                    unionOfIntervals a d
                        |> unionToString
                        |> Expect.equal "{ [1, 2), (2, 5] }"
            , test "intersecting intervals" <|
                -- [3, 4] ∪ [3, 5] = { [3, 4] }
                \_ ->
                    unionOfIntervals b d
                        |> unionToString
                        |> Expect.equal "{ (2, 5] }"
            ]
        ]
