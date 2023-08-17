module Interval exposing
    ( Interval
    , Bound
    , interval
    , degenerate
    , empty
    , leftBounded
    , rightBounded
    , unbounded
    , hull
    , intersection
    , intervalToString
    , interior
    , closure
    , subtract
    , upperBoundValue
    , lowerBoundValue
    , adjoins
    , intersects
    , intersectsPoint
    , isBounded
    , isDegenerate
    , isEmpty
    , isLeftBounded
    , isRightBounded
    , isLeftOpen
    , isRightOpen
    , leftBound, rightBound
    , plus, negate, minus
    , excludes, includes
    )

{-| A representation of numeric intervals (also known as _ranges_.)


# Types

@docs Interval
@docs Bound


# Constructors

`interval` is the primary constructor; the others are just for convenience.

@docs interval
@docs degenerate
@docs empty
@docs leftBounded
@docs rightBounded
@docs unbounded


# Operations on Intervals

@docs hull
@docs intersection
@docs intervalToString
@docs interior
@docs closure
@docs subtract
@docs upperBoundValue
@docs lowerBoundValue


# Tests on Intervals

@docs adjoins
@docs intersects
@docs intersectsPoint
@docs isBounded
@docs isDegenerate
@docs isEmpty
@docs isLeftBounded
@docs isRightBounded
@docs isLeftOpen
@docs isRightOpen
@docs leftBound, rightBound


# Arithmetic functions

@docs plus, negate, minus


# Deprecated functions

@docs excludes, includes


# Related reading

  - [Interval](https://en.wikipedia.org/wiki/Interval_(mathematics))
  - [Interval tree](https://en.wikipedia.org/wiki/Interval_tree)
  - [Allen's interval algebra](https://en.wikipedia.org/wiki/Allen%27s_interval_algebra)

-}

import Bound exposing (Bound(..))
import String


{-| An interval over the reals. May be over either the Ordinary Reals `(-∞, +∞)` or
the Extended Reals `[-∞, +∞]`.
`Bounded x y` will always satisfy `x < y`. (`x == y` is either degenerate or empty)

Opaque type.

-}
type Interval
    = Bounded Bound Bound
    | Degenerate Float
    | Empty


{-| Represents an upper or lower, closed or open endpoint of an Interval.
This encompasses the "endpoints" of unbounded intervals when the bound value
is either of the `Infinity` values in the floating point spec.

Deprecated, use `Bound.Bound` directly.

-}
type alias Bound =
    Bound.Bound


{-| An inclusive endpoint of an interval.

Deprecated, use `Bound.Inclusive` directly.

-}
includes : Float -> Bound
includes n =
    Inclusive n


{-| An exclusive endpoint of an interval.

Deprecated, use `Bound.Exclusive` directly.

-}
excludes : Float -> Bound
excludes n =
    Exclusive n


{-| An empty Interval.
-}
empty : Interval
empty =
    Empty


{-| A degenerate Interval.
-}
degenerate : Float -> Interval
degenerate n =
    Degenerate n


{-| An unbounded Interval over the [Extended Reals.] `[-∞, +∞]`

[Extended Reals.]: https://en.wikipedia.org/wiki/Interval_(mathematics)#Infinite_endpoints

-}
unbounded : Interval
unbounded =
    interval (Inclusive <| -1 / 0) (Inclusive <| 1 / 0)


{-| Convenience function for a right-bounded Interval (from -∞ included to some n)
-}
rightBounded : Bound -> Interval
rightBounded b =
    interval (Inclusive <| -1 / 0) b


{-| Convenience function for a left-bounded Interval (from some n to +∞ included)
-}
leftBounded : Bound -> Interval
leftBounded b =
    interval b (Inclusive <| 1 / 0)


{-| Constructs an `Interval` from two Bounds.
-}
interval : Bound -> Bound -> Interval
interval i j =
    let
        t : Float
        t =
            Bound.value i

        u : Float
        u =
            Bound.value j
    in
    if isNaN t || isNaN u then
        Empty

    else if t == u then
        case ( i, j ) of
            ( Inclusive _, Inclusive _ ) ->
                Degenerate t

            _ ->
                Empty

    else if t < u then
        Bounded i j

    else
        Empty


{-| Return a String representation of an Interval.
-}
intervalToString : Interval -> String
intervalToString interval_val =
    case interval_val of
        Empty ->
            "{}"

        Degenerate n ->
            "{" ++ String.fromFloat n ++ "}"

        Bounded x y ->
            let
                left =
                    case x of
                        Exclusive n ->
                            "(" ++ String.fromFloat n

                        Inclusive n ->
                            "[" ++ String.fromFloat n

                right =
                    case y of
                        Exclusive n ->
                            String.fromFloat n ++ ")"

                        Inclusive n ->
                            String.fromFloat n ++ "]"
            in
            left ++ ", " ++ right


{-| The intersection of two intervals. If the intervals overlap, this is the common part.
If not, this is the empty interval.
-}
intersection : Interval -> Interval -> Interval
intersection a b =
    case ( a, b ) of
        ( Empty, _ ) ->
            Empty

        ( _, Empty ) ->
            Empty

        ( Degenerate x, Degenerate y ) ->
            if x == y then
                Degenerate x

            else
                Empty

        ( Degenerate w, Bounded y z ) ->
            interval
                (Bound.maxOuter (includes w) y)
                (Bound.minOuter (includes w) z)

        ( Bounded w x, Degenerate y ) ->
            interval
                (Bound.maxOuter w (includes y))
                (Bound.minOuter x (includes y))

        ( Bounded w x, Bounded y z ) ->
            interval
                (Bound.maxOuter w y)
                (Bound.minOuter x z)


{-| The convex hull of two intervals. This is similar to union in that
it includes all the points of the component intervals, and for
non-overlapping intervals, the points between them.
-}
hull : Interval -> Interval -> Interval
hull a b =
    case ( a, b ) of
        ( Empty, _ ) ->
            b

        ( _, Empty ) ->
            a

        ( Degenerate x, Degenerate y ) ->
            interval (includes <| min x y) (includes <| max x y)

        ( Degenerate w, Bounded y z ) ->
            interval
                (Bound.minInner (includes w) y)
                (Bound.maxInner (includes w) z)

        ( Bounded w x, Degenerate y ) ->
            interval
                (Bound.minInner w (includes y))
                (Bound.maxInner x (includes y))

        ( Bounded w x, Bounded y z ) ->
            interval
                (Bound.minInner w y)
                (Bound.maxInner x z)


{-| Extract the value of the lower bound of an Interval.
-}
lowerBoundValue : Interval -> Maybe Float
lowerBoundValue a =
    case a of
        Empty ->
            Nothing

        Degenerate n ->
            Just n

        Bounded l _ ->
            Just <| Bound.value l


{-| Extract the value of the upper bound of an Interval.
-}
upperBoundValue : Interval -> Maybe Float
upperBoundValue a =
    case a of
        Empty ->
            Nothing

        Degenerate n ->
            Just n

        Bounded _ u ->
            Just <| Bound.value u


{-| Do these two intervals intersect?

    let
        a = interval (includes 1) (excludes 3)
        b = interval (includes 2) (includes 4)
        c = interval (includes 3) (includes 4)
    in
        [ intersects a b = True
        , intersects a c = False
        ]

-}
intersects : Interval -> Interval -> Bool
intersects a b =
    case ( a, b ) of
        ( Empty, _ ) ->
            False

        ( _, Empty ) ->
            False

        ( Degenerate x, Degenerate y ) ->
            x == y

        ( Degenerate _, Bounded _ _ ) ->
            intersection a b == a

        ( Bounded _ _, Degenerate _ ) ->
            intersection a b == b

        ( Bounded _ _, Bounded _ _ ) ->
            intersection a b /= empty


{-| Are these two intervals adjoins? I.e., do they share an upper-lower or
lower-upper bound, exactly one of which is closed, and do not intersect each other?

    let
        a = interval (includes 1) (excludes 3)  -- [1, 3)
        b = interval (includes 2) (includes 4)  -- [3, 4]
        c = interval (excludes 3) (includes 4)  -- (3, 4]
        d = interval (includes 2) (includes 3)  -- [2, 3]
    in
        [ adjoins a b = True
        , adjoins a c = False
        , adjoins a d = False
        ]

-}
adjoins : Interval -> Interval -> Bool
adjoins a b =
    case ( a, b ) of
        ( Empty, _ ) ->
            False

        ( _, Empty ) ->
            False

        ( Degenerate _, Degenerate _ ) ->
            False

        ( Degenerate w, Bounded y z ) ->
            (Bound.isOpen y && w == Bound.value y) || (Bound.isOpen z && w == Bound.value z)

        ( Bounded w x, Degenerate y ) ->
            (Bound.isOpen w && y == Bound.value w) || (Bound.isOpen x && y == Bound.value x)

        ( Bounded w x, Bounded y z ) ->
            let
                ( ( wOpen, xOpen ), ( yOpen, zOpen ) ) =
                    ( ( Bound.isOpen w, Bound.isOpen x ), ( Bound.isOpen y, Bound.isOpen z ) )

                upperLowerMatch =
                    (xOpen |> xor yOpen)
                        && (Bound.value x == Bound.value y)

                lowerUpperMatch =
                    (wOpen |> xor zOpen)
                        && (Bound.value w == Bound.value z)
            in
            not (a |> intersects b) && (upperLowerMatch || lowerUpperMatch)


{-| Does this interval contain the given point?

    let
        a = interval (includes 1) (excludes 3)
    in
        [ intersectsPoint a 0 = False
        , intersectsPoint a 1 = True
        , intersectsPoint a 3 = False
        ]

-}
intersectsPoint : Interval -> Float -> Bool
intersectsPoint a n =
    case a of
        Empty ->
            False

        Degenerate x ->
            x == n

        Bounded _ _ ->
            intersection a (degenerate n) == degenerate n


{-| Does this interval have finite bounds?
-}
isBounded : Interval -> Bool
isBounded a =
    case a of
        Empty ->
            False

        Degenerate x ->
            not <| isInfinite x

        Bounded x y ->
            not <| isInfinite (Bound.value x) || isInfinite (Bound.value y)


{-| Is this a degenerate (point-valued) interval?
-}
isDegenerate : Interval -> Bool
isDegenerate a =
    case a of
        Degenerate _ ->
            True

        Empty ->
            False

        Bounded _ _ ->
            False


{-| Is this an empty interval?
-}
isEmpty : Interval -> Bool
isEmpty a =
    case a of
        Empty ->
            True

        Degenerate _ ->
            False

        Bounded _ _ ->
            False


{-| Does this interval have a finite lower bound, and an infinite upper bound?
-}
isLeftBounded : Interval -> Bool
isLeftBounded a =
    case a of
        Empty ->
            False

        Degenerate _ ->
            False

        Bounded x y ->
            (not <| isInfinite (Bound.value x)) && isInfinite (Bound.value y)


{-| Does this interval have a finite upper bound, and an infinite lower bound?
-}
isRightBounded : Interval -> Bool
isRightBounded a =
    case a of
        Empty ->
            False

        Degenerate _ ->
            False

        Bounded x y ->
            (not <| isInfinite (Bound.value y)) && isInfinite (Bound.value x)


{-| Is this interval unbounded?
-}
isUnbounded : Interval -> Bool
isUnbounded a =
    case a of
        Empty ->
            False

        Degenerate _ ->
            False

        Bounded lower upper ->
            (isInfinite <| Bound.value lower) && (isInfinite <| Bound.value upper)


{-| Is the lower bound of this interval open?
-}
isLeftOpen : Interval -> Bool
isLeftOpen a =
    case a of
        Empty ->
            False

        Degenerate _ ->
            False

        Bounded lower _ ->
            Bound.isOpen lower


{-| Is the upper bound of this interval open?
-}
isRightOpen : Interval -> Bool
isRightOpen a =
    case a of
        Empty ->
            False

        Degenerate _ ->
            False

        Bounded _ upper ->
            Bound.isOpen upper


{-| Returns the left (lower) bound of the interval, returns `Nothing` if it's empty.

    leftBound (interval (includes 0) (excludes 1)) == Just (includes 0)

-}
leftBound : Interval -> Maybe Bound
leftBound a =
    case a of
        Empty ->
            Nothing

        Degenerate f ->
            Just <| Inclusive f

        Bounded l _ ->
            Just l


{-| Returns the right (upper) bound of the interval, returns `Nothing` if it's empty.

    rightBound (interval (includes 0) (excludes 1)) == Just (excludes 1)

-}
rightBound : Interval -> Maybe Bound
rightBound a =
    case a of
        Empty ->
            Nothing

        Degenerate f ->
            Just <| Inclusive f

        Bounded _ r ->
            Just r


{-| Returns the largest open interval contained within a.

    -- `interior([x, y]) == (x, y)`
    interior (interval (includes 0) (includes 2)) == interval (excludes 0) (excludes 2)

-}
interior : Interval -> Interval
interior a =
    case a of
        Empty ->
            empty

        Degenerate _ ->
            empty

        Bounded x y ->
            let
                t =
                    Bound.value x

                u =
                    Bound.value y
            in
            interval (excludes t) (excludes u)


{-| Returns the smallest closed interval containing a.

    -- `closure((x, y)) == [x, y]`
    closure (interval (excludes 0) (excludes 2)) == interval (includes 0) (includes 2)

-}
closure : Interval -> Interval
closure a =
    case a of
        Empty ->
            empty

        Degenerate _ ->
            a

        Bounded x y ->
            let
                t =
                    Bound.value x

                u =
                    Bound.value y
            in
            interval (includes t) (includes u)


{-| Subtract the second interval from the first one, returning a list of the parts of
the first that did not intersect with the second.

E.g.:

  - [1, 3) - (1, 2] = [ {1}, (2, 3) ]

-}
subtract : Interval -> Interval -> List Interval
subtract a b =
    if a |> intersects b then
        case ( a, b ) of
            ( _, Empty ) ->
                [ a ]

            ( Empty, _ ) ->
                []

            ( Degenerate _, _ ) ->
                []

            ( Bounded w x, Degenerate y ) ->
                -- ?w, x? - {y} = [ ?w, y), (y, x? ]
                [ interval w (excludes y)
                , interval (excludes y) x
                ]

            ( Bounded w x, Bounded y z ) ->
                let
                    left =
                        if Bound.minInner w y == w && w /= y then
                            [ interval w (Bound.invert y) ]

                        else
                            []

                    right =
                        if Bound.maxInner x z == x && x /= z then
                            [ interval (Bound.invert z) x ]

                        else
                            []
                in
                List.append left right

    else
        [ a ]


plus : Interval -> Interval -> Interval
plus l r =
    let
        boundPlusFloat : Bound -> Float -> Bound
        boundPlusFloat bound f =
            case bound of
                Inclusive n ->
                    Inclusive (f + n)

                Exclusive n ->
                    Exclusive (f + n)

        boundPlusBound : Bound -> Bound -> Bound
        boundPlusBound lbound rbound =
            case ( lbound, rbound ) of
                ( Inclusive lb, Inclusive rb ) ->
                    Inclusive (lb + rb)

                ( Inclusive lb, Exclusive rb ) ->
                    Exclusive (lb + rb)

                ( Exclusive lb, Inclusive rb ) ->
                    Exclusive (lb + rb)

                ( Exclusive lb, Exclusive rb ) ->
                    Exclusive (lb + rb)
    in
    case ( l, r ) of
        ( Empty, _ ) ->
            Empty

        ( _, Empty ) ->
            Empty

        ( Degenerate lf, Degenerate rf ) ->
            Degenerate (lf + rf)

        ( Bounded ll lu, Degenerate rf ) ->
            Bounded (boundPlusFloat ll rf) (boundPlusFloat lu rf)

        ( Degenerate lf, Bounded rl ru ) ->
            Bounded (boundPlusFloat rl lf) (boundPlusFloat ru lf)

        ( Bounded ll lu, Bounded rl ru ) ->
            Bounded (boundPlusBound ll rl) (boundPlusBound lu ru)


negate : Interval -> Interval
negate l =
    case l of
        Empty ->
            Empty

        Bounded lower upper ->
            Bounded (Bound.negate upper) (Bound.negate lower)

        Degenerate f ->
            Degenerate -f


minus : Interval -> Interval -> Interval
minus l r =
    plus l (negate r)
