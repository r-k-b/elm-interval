module TestUnions exposing (suite)

import Expect exposing (Expectation)
import Test exposing (Test, describe, skip, test, todo)
import Interval
    exposing
        ( degenerate
        , empty
        , excludes
        , includes
        , interval
        , intervalToString
        , unbounded
        )
import Union
    exposing
        ( fromInterval
        , fromIntervals
        , intersection
        , intersectionWithInterval
        , subtract
        , subtractInterval
        , subtractUnions
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


f =
    -- [1, 3)
    interval (includes 1) (excludes 3)


g =
    -- [4, 6]
    interval (includes 4) (includes 6)


h =
    -- [7, 8]
    interval (includes 7) (includes 8)


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
        , describe "intersections of single intervals with unions"
            [ test "[5, 6) ∩ { [1, 2), [3, 4] }" <|
                --  e          a       b
                \_ ->
                    intersectionWithInterval e (unionOfIntervals a b)
                        |> unionToString
                        |> Expect.equal "{}"
            , test "[-Infinity, Infinity] ∩ { [1, 2), [3, 4] }" <|
                --                            a       b
                \_ ->
                    intersectionWithInterval unbounded (unionOfIntervals a b)
                        |> unionToString
                        |> Expect.equal "{ [1, 2), [3, 4] }"
            , test "(2, 5] ∩ { [1, 3), [4, 6] }" <|
                --  d          f       g
                \_ ->
                    intersectionWithInterval d (unionOfIntervals f g)
                        |> unionToString
                        |> Expect.equal "{ (2, 3), [4, 5] }"
            ]
        , describe "intersections of unions"
            [ test "{ [1, 2), [3, 4] } ∩ { [5, 6), [7, 8] }" <|
                --    a       b            e       h
                \_ ->
                    intersection (unionOfIntervals a b) (fromIntervals [ e, h ])
                        |> unionToString
                        |> Expect.equal "{}"
            , test "{ [1, 2), [3, 4] } ∩ { [5, 6) }" <|
                --    a       b            e
                \_ ->
                    intersection (unionOfIntervals a b) (fromInterval e)
                        |> unionToString
                        |> Expect.equal "{}"
            , test "{ [1, 2), [3, 4] } ∩ { [2, 3] }" <|
                --    a       b            c
                \_ ->
                    intersection (unionOfIntervals a b) (fromInterval c)
                        |> unionToString
                        |> Expect.equal "{ {3} }"
            , test "{ [1, 2), [3, 4] } ∩ { [-Infinity, Infinity] }" <|
                --    a       b
                \_ ->
                    intersection (unionOfIntervals a b) (fromInterval unbounded)
                        |> unionToString
                        |> Expect.equal "{ [1, 2), [3, 4] }"
            , test "{ [1, 3), [4, 6] } ∩ { (2, 5] }" <|
                --    f       g            d
                \_ ->
                    intersection (unionOfIntervals f g) (fromInterval d)
                        |> unionToString
                        |> Expect.equal "{ (2, 3), [4, 5] }"
            ]
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
        , describe "subtracting intervals from intervals"
            [ test "prime example" <|
                -- [1, 3) - (1, 2]
                let
                    expected =
                        "{ {1}, (2, 3) }"

                    a =
                        interval (includes 1) (excludes 3)

                    b =
                        interval (excludes 1) (includes 2)
                in
                    \_ ->
                        subtract a b
                            |> unionToString
                            |> Expect.equal expected
            ]
        , describe "subtracting intervals from unions"
            [ test "prime example" <|
                -- { [1, 3), (5, 8] } - (1, 3)
                --   f
                let
                    expected =
                        "{ {1}, (5, 8] }"

                    i =
                        fromIntervals
                            [ f
                            , interval (excludes 5) (includes 8)
                            ]

                    u =
                        interval (excludes 1) (excludes 3)
                in
                    \_ ->
                        subtractInterval u i
                            |> unionToString
                            |> Expect.equal expected
            ]
        , describe "subtracting unions"
            [ test "prime example" <|
                -- { [1, 3), (5, 8] } - { (1, 3), [6, 7] }
                --   f
                let
                    expected =
                        "{ {1}, (5, 6), (7, 8] }"

                    ua =
                        fromIntervals
                            [ f
                            , interval (excludes 5) (includes 8)
                            ]

                    ub =
                        fromIntervals
                            [ interval (excludes 1) (excludes 3)
                            , interval (includes 6) (includes 7)
                            ]
                in
                    \_ ->
                        subtractUnions ua ub
                            |> unionToString
                            |> Expect.equal expected
            ]
        ]
