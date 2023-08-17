module TestIntervals exposing (suite)

import Bound exposing (Bound)
import Expect
import Fuzz exposing (Fuzzer)
import Interval exposing (Interval)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "Arithmetic"
        [ fuzz intervalFuzzer "Negation is an involution" <|
            \interval ->
                interval
                    |> Interval.negate
                    |> Interval.negate
                    |> Expect.equal interval
        , fuzz intervalFuzzer "Zero is an identity for plus (left)" <|
            \interval ->
                Interval.plus (Interval.degenerate 0) interval
                    |> Expect.equal interval
        , fuzz intervalFuzzer "Zero is an identity for plus (right)" <|
            \interval ->
                Interval.plus interval (Interval.degenerate 0)
                    |> Expect.equal interval
        ]


intervalFuzzer : Fuzzer Interval
intervalFuzzer =
    Fuzz.map2 Interval.interval boundFuzzer boundFuzzer


boundFuzzer : Fuzzer Bound
boundFuzzer =
    Fuzz.oneOf
        [ Fuzz.map Bound.Inclusive Fuzz.float
        , Fuzz.map Bound.Exclusive Fuzz.float
        ]
