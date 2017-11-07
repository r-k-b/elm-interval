module Example exposing (..)

import Expect exposing (Expectation)
import Test exposing (describe, test, Test)
import Interval exposing (empty, excludes, includes, intersection, interval, intervalToString, unbounded)


{-
   Empty
   Unbounded
   Degenerate Float
   LeftBounded (Bound Float)
   RightBounded (Bound Float)
   Bounded (Bound Float) (Bound Float)
-}


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
                , test "two degens" <|
                    \_ ->
                        intersection b empty
                            |> intervalToString
                            |> Expect.equal "{}"
                ]
            )
        ]
