module Example exposing (..)

import Expect exposing (Expectation)
import Test exposing (describe, test, Test)
import Interval
    exposing
        ( degenerate
        , empty
        , excludes
        , hull
        , includes
        , intersection
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
        ]
