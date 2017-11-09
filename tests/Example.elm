module Example exposing (..)

import Expect exposing (Expectation)
import Test exposing (describe, test, Test)
import Interval
    exposing
        ( adjoins
        , closure
        , degenerate
        , empty
        , excludes
        , hull
        , includes
        , interior
        , intersection
        , intersects
        , intersectsPoint
        , interval
        , intervalToString
        , leftBounded
        , rightBounded
        , unbounded
        )


suite : Test
suite =
    describe "intervals"
        [ describe "toString examples"
            [ test "explicit empty interval" <|
                \_ ->
                    empty
                        |> intervalToString
                        |> Expect.equal "{}"
            , test "unbounded interval" <|
                \_ ->
                    unbounded
                        |> intervalToString
                        |> Expect.equal "[-Infinity, Infinity]"
            , test "degenerate interval" <|
                \_ ->
                    interval (includes 1) (includes 1)
                        |> intervalToString
                        |> Expect.equal "{1}"
            , test "implicit empty interval" <|
                \_ ->
                    interval (includes 1) (excludes 1)
                        |> intervalToString
                        |> Expect.equal "{}"
            , test "left bounded interval" <|
                \_ ->
                    interval (includes 1) (includes <| 1 / 0)
                        |> intervalToString
                        |> Expect.equal "[1, Infinity]"
            , test "right bounded interval" <|
                \_ ->
                    interval (includes <| -1 / 0) (includes 1)
                        |> intervalToString
                        |> Expect.equal "[-Infinity, 1]"
            , test "bounded interval (normal)" <|
                \_ ->
                    interval (includes -1) (includes 1)
                        |> intervalToString
                        |> Expect.equal "[-1, 1]"
            , test "bounded interval (reversed)" <|
                \_ ->
                    interval (includes 1) (includes -1)
                        |> intervalToString
                        |> Expect.equal "{}"
            ]
        , describe "intersections"
            (let
                a =
                    interval (includes -1) (includes 3)

                b =
                    interval (includes 2) (includes 9)

                c =
                    interval (includes 5) (includes 8)
             in
                [ test "empty intersection" <|
                    \_ ->
                        intersection a c
                            |> intervalToString
                            |> Expect.equal "{}"
                , test "partial intersection" <|
                    \_ ->
                        intersection a b
                            |> intervalToString
                            |> Expect.equal "[2, 3]"
                , test "totally enclosed 1" <|
                    \_ ->
                        intersection b c
                            |> intervalToString
                            |> Expect.equal "[5, 8]"
                , test "totally enclosed 2" <|
                    \_ ->
                        intersection c b
                            |> intervalToString
                            |> Expect.equal "[5, 8]"
                , test "with left empty" <|
                    \_ ->
                        intersection empty b
                            |> intervalToString
                            |> Expect.equal "{}"
                , test "with right empty" <|
                    \_ ->
                        intersection b empty
                            |> intervalToString
                            |> Expect.equal "{}"
                , test "two mismatched degens" <|
                    \_ ->
                        intersection (degenerate 2) (degenerate 3)
                            |> intervalToString
                            |> Expect.equal "{}"
                , test "two matching degens" <|
                    \_ ->
                        intersection (degenerate 2) (degenerate 2)
                            |> intervalToString
                            |> Expect.equal "{2}"
                , test "bounded ∩ unbounded" <|
                    \_ ->
                        intersection c unbounded
                            |> intervalToString
                            |> Expect.equal "[5, 8]"
                , test "unbounded ∩ bounded" <|
                    \_ ->
                        intersection unbounded b
                            |> intervalToString
                            |> Expect.equal "[2, 9]"
                , test "degen & bounded (exclusive)" <|
                    \_ ->
                        intersection (degenerate 1) (interval (excludes 1) (includes 2))
                            |> intervalToString
                            |> Expect.equal "{}"
                ]
            )
        , describe "hulls"
            (let
                a =
                    interval (includes -1) (includes 3)

                b =
                    interval (includes 2) (includes 9)

                c =
                    interval (includes 5) (includes 8)
             in
                [ test "hull of separate degens" <|
                    \_ ->
                        hull (degenerate 1) (degenerate 3)
                            |> intervalToString
                            |> Expect.equal "[1, 3]"
                , test "hull of same degens" <|
                    \_ ->
                        hull (degenerate 1) (degenerate 1)
                            |> intervalToString
                            |> Expect.equal "{1}"
                , test "hull of overlapping" <|
                    \_ ->
                        hull a b
                            |> intervalToString
                            |> Expect.equal "[-1, 9]"
                , test "right-bounded and degen" <|
                    \_ ->
                        hull (rightBounded <| includes 1) (degenerate 3)
                            |> intervalToString
                            |> Expect.equal "[-Infinity, 3]"
                , test "left-bounded and degen" <|
                    \_ ->
                        hull (leftBounded <| includes 1) (degenerate -3)
                            |> intervalToString
                            |> Expect.equal "[-3, Infinity]"
                ]
            )
        , describe "intersection tests"
            [ test "mismatched degens" <|
                \_ ->
                    intersects (degenerate 1) (degenerate 3)
                        |> Expect.equal False
            , test "matching degens" <|
                \_ ->
                    intersects (degenerate 1) (degenerate 1)
                        |> Expect.equal True
            , test "degen & bounded (inclusive)" <|
                \_ ->
                    intersects (degenerate 1) (interval (includes 1) (includes 2))
                        |> Expect.equal True
            , test "degen & bounded (exclusive)" <|
                \_ ->
                    intersects (degenerate 1) (interval (excludes 1) (includes 2))
                        |> Expect.equal False
            ]
        , describe "point intersection tests"
            (let
                a =
                    interval (includes 1) (excludes 3)
             in
                [ test "a" <|
                    \_ ->
                        intersectsPoint a 0
                            |> Expect.equal False
                , test "b" <|
                    \_ ->
                        intersectsPoint a 1
                            |> Expect.equal True
                , test "c" <|
                    \_ ->
                        intersectsPoint a 3
                            |> Expect.equal False
                , test "unbounded a" <|
                    \_ ->
                        intersectsPoint unbounded 3
                            |> Expect.equal True
                , test "unbounded b" <|
                    \_ ->
                        intersectsPoint unbounded (1 / 0)
                            |> Expect.equal True
                , test "unbounded c" <|
                    \_ ->
                        intersectsPoint unbounded (-1 / 0)
                            |> Expect.equal True
                ]
            )
        , describe "closure and interior"
            (let
                a =
                    interval (includes 0) (includes 2)

                b =
                    interval (excludes 0) (excludes 2)
             in
                [ test "interior" <|
                    \_ ->
                        (b == interior a)
                            |> Expect.equal True
                , test "closure" <|
                    \_ ->
                        (a == closure b)
                            |> Expect.equal True
                , test "interior of degen" <|
                    \_ ->
                        interior (degenerate 1)
                            |> intervalToString
                            |> Expect.equal "{}"
                , test "closure of degen" <|
                    \_ ->
                        closure (degenerate 1)
                            |> intervalToString
                            |> Expect.equal "{1}"
                , test "closure of empty" <|
                    \_ ->
                        closure empty
                            |> intervalToString
                            |> Expect.equal "{}"
                ]
            )
        , describe "adjoint intervals"
            (let
                a =
                    -- [1, 3)
                    interval (includes 1) (excludes 3)

                b =
                    -- [3, 4]
                    interval (includes 3) (includes 4)

                c =
                    -- (3, 4]
                    interval (includes 5) (includes 8)

                d =
                    -- [2, 3]
                    interval (includes 5) (includes 8)
             in
                [ test "adj [1, 3) [3, 4]" <|
                    \_ ->
                        adjoins a b
                            |> Expect.equal True
                , test "adj [1, 3) (3, 4]" <|
                    \_ ->
                        adjoins a c
                            |> Expect.equal False
                , test "adj [1, 3) [2, 3]" <|
                    \_ ->
                        adjoins a d
                            |> Expect.equal False
                , test "adj [2, 3] [3, 4]" <|
                    \_ ->
                        adjoins b d
                            |> Expect.equal False
                ]
            )
        ]
