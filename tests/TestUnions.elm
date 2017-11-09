module TestUnions exposing (suite)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test, todo)
import Interval
    exposing
        ( degenerate
        , empty
        , excludes
        , includes
        , interval
        , intervalToString
        )
import Union
    exposing
        ( fromInterval
        , fromIntervals
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


e =
    -- [5, 6)
    interval (includes 5) (excludes 6)


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
                -- a        b
                \_ ->
                    unionOfIntervals a b
                        |> unionToString
                        |> Expect.equal "{ [1, 2), [3, 4] }"
            , test "adjoint intervals" <|
                -- [1, 2) ∪ [2, 3] = { [1, 3] }
                -- a        c
                \_ ->
                    unionOfIntervals a c
                        |> unionToString
                        |> Expect.equal "{ [1, 3] }"
            , test "degenerate-adjoint intervals" <|
                -- [1, 2) ∪ {2} = { [1, 2] }
                -- a
                \_ ->
                    unionOfIntervals a (degenerate 2)
                        |> unionToString
                        |> Expect.equal "{ [1, 2] }"
            , test "degenerate-adjoint intervals 2" <|
                -- (2, 5] ∪ {2} = { [2, 5] }
                -- d
                \_ ->
                    unionOfIntervals d (degenerate 2)
                        |> unionToString
                        |> Expect.equal "{ [2, 5] }"
            , test "not-quite-adjoint intervals" <|
                -- [1, 2) ∪ (2, 5] = { [1, 2), (2, 5] }
                \_ ->
                    unionOfIntervals a d
                        |> unionToString
                        |> Expect.equal "{ [1, 2), (2, 5] }"
            , test "intersecting intervals" <|
                \_ ->
                    unionOfIntervals b d
                        |> unionToString
                        |> Expect.equal "{ (2, 5] }"
            ]
        , describe "unions of unions"
            [ test "all separate intervals" <|
                -- { [1, 2), [3, 4] } ∪ { [5, 6) }
                \_ ->
                    union (unionOfIntervals a b) (fromInterval e)
                        |> unionToString
                        |> Expect.equal "{ [1, 2), [3, 4], [5, 6) }"
            , test "no non-intersecting intervals" <|
                -- { [1, 2), [3, 4] } ∪ { [2, 3] }
                --   a       b            c
                \_ ->
                    union (unionOfIntervals a b) (fromInterval c)
                        |> unionToString
                        |> Expect.equal "{ [1, 4] }"
            ]
        , describe "intersections of unions"
            [ todo "{ [1, 2), [3, 4] } ∪ { [5, 6) }" ]
        , describe "unions from arbitrary unordered lists"
            [ test "already ordered, all valid, no intersections" <|
                -- [ [1, 2), [3, 4], [5, 6), {7} ]
                --   a       b       e
                \_ ->
                    fromIntervals [ a, b, e, degenerate 7 ]
                        |> unionToString
                        |> Expect.equal "{ [1, 2), [3, 4], [5, 6), {7} }"
            , test "unordered, all valid, no intersections" <|
                -- [ [5, 6), [3, 4], {7}, [1, 2) ]
                --   e       b            a
                \_ ->
                    fromIntervals [ e, b, degenerate 7, a ]
                        |> unionToString
                        |> Expect.equal "{ [1, 2), [3, 4], [5, 6), {7} }"
            , test "unordered, all valid, with intersections" <|
                -- [ {7}, [2, 3], (2, 5], {4}, [1, 2) ]
                --        c       d            a
                \_ ->
                    fromIntervals [ degenerate 7, c, d, degenerate 4, a ]
                        |> unionToString
                        |> Expect.equal "{ [1, 5], {7} }"
            , test "ordered, all valid, with a slot for a degenerate" <|
                -- [ [1, 2), (2, 5] ]
                --   a       d
                \_ ->
                    fromIntervals [ a, d ]
                        |> unionToString
                        |> Expect.equal "{ [1, 2), (2, 5] }"
            , test "ordered, all valid, slot-filling degenerates" <|
                -- [ [1, 2), {2}, (2, 5] ]
                --   a            d
                \_ ->
                    fromIntervals [ a, degenerate 2, d ]
                        |> unionToString
                        |> Expect.equal "{ [1, 5] }"
            , test "unordered, all valid, slot-filling degenerates" <|
                -- [ [1, 2), (2, 5], {2} ]
                --   a       d
                \_ ->
                    fromIntervals [ a, d, degenerate 2 ]
                        |> unionToString
                        |> Expect.equal "{ [1, 5] }"
            ]
        ]
