module Interval
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

{-| A representation of numeric intervals (also known as *ranges*.)

See also [Wikipedia on intervals][WP].
[WP]: <https://en.wikipedia.org/wiki/Interval_(mathematics)>

@docs degenerate
@docs empty
@docs excludes
@docs hull
@docs includes
@docs intersection
@docs interval
@docs intervalToString
@docs leftBounded
@docs rightBounded
@docs unbounded

<https://en.wikipedia.org/wiki/Allen%27s_interval_algebra> ?
<https://en.wikipedia.org/wiki/Interval_tree> ?

-}


{-| An interval over the extended reals.
`Bounded x y` will always satisfy `x < y`. (`x == y` is either degenerate or empty)

(Opaque type; do not export)

-}
type Interval
    = Bounded Bound Bound
    | Degenerate Float
    | Empty


type Bound
    = Inclusive Float
    | Exclusive Float


{-| An inclusive endpoint of an interval.
-}
includes : Float -> Bound
includes n =
    Inclusive n


{-| An exclusive endpoint of an interval.
-}
excludes : Float -> Bound
excludes n =
    Exclusive n


boundValue : Bound -> Float
boundValue b =
    case b of
        Inclusive n ->
            n

        Exclusive n ->
            n


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


{-| An unbounded Interval.
-}
unbounded : Interval
unbounded =
    interval (Inclusive <| -1 / 0) (Inclusive <| 1 / 0)


{-| Convenience function for a right-bounded Interval (from -∞ to some n)
-}
rightBounded : Bound -> Interval
rightBounded b =
    interval (Inclusive <| -1 / 0) b


{-| Convenience function for a left-bounded Interval (from some n to +∞)
-}
leftBounded : Bound -> Interval
leftBounded b =
    interval b (Inclusive <| 1 / 0)


{-| Constructs an `Interval` from two Bounds.
-}
interval : Bound -> Bound -> Interval
interval i j =
    let
        t =
            boundValue i

        u =
            boundValue j
    in
        case (t == u) of
            True ->
                case ( i, j ) of
                    ( Inclusive _, Inclusive _ ) ->
                        Degenerate t

                    ( _, _ ) ->
                        Empty

            False ->
                case (t < u) of
                    True ->
                        Bounded i j

                    False ->
                        Empty


{-| Return a String representation of an Interval.
-}
intervalToString : Interval -> String
intervalToString interval =
    case interval of
        Empty ->
            "{}"

        Degenerate n ->
            "{" ++ toString n ++ "}"

        Bounded x y ->
            let
                left =
                    case x of
                        Exclusive n ->
                            "(" ++ toString n

                        Inclusive n ->
                            "[" ++ toString n

                right =
                    case y of
                        Exclusive n ->
                            toString n ++ ")"

                        Inclusive n ->
                            toString n ++ "]"
            in
                left ++ ", " ++ right


{-| Return the minimum of two Bounds.
NB:

  - `[n < (n`

-}
minBound : Bound -> Bound -> Bound
minBound a b =
    let
        x =
            boundValue a

        y =
            boundValue b
    in
        case (x < y) of
            True ->
                a

            False ->
                case (y < x) of
                    True ->
                        b

                    False ->
                        -- x == y
                        case ( a, b ) of
                            ( Exclusive _, Exclusive _ ) ->
                                Exclusive x

                            ( _, _ ) ->
                                Inclusive x


{-| Return the maximum of two Bounds.
NB:

  - `n) < n]`

-}
maxBound : Bound -> Bound -> Bound
maxBound a b =
    let
        x =
            boundValue a

        y =
            boundValue b
    in
        case (x < y) of
            True ->
                b

            False ->
                case (y < x) of
                    True ->
                        a

                    False ->
                        -- x == y
                        case ( a, b ) of
                            ( Exclusive _, Exclusive _ ) ->
                                Exclusive x

                            ( _, _ ) ->
                                Inclusive x


{-| The intersection of two intervals. If the intervals overlap, this is the common part. If not, this is the empty interval.
-}
intersection : Interval -> Interval -> Interval
intersection a b =
    case ( a, b ) of
        ( Empty, _ ) ->
            Empty

        ( _, Empty ) ->
            Empty

        ( Degenerate x, Degenerate y ) ->
            case (x == y) of
                True ->
                    Degenerate x

                False ->
                    Empty

        ( Degenerate w, Bounded y z ) ->
            interval
                (maxBound (includes w) y)
                (minBound (includes w) z)

        ( Bounded w x, Degenerate y ) ->
            interval
                (maxBound w (includes y))
                (minBound x (includes y))

        ( Bounded w x, Bounded y z ) ->
            interval
                (maxBound w y)
                (minBound x z)


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
                (minBound (includes w) y)
                (maxBound (includes w) z)

        ( Bounded w x, Degenerate y ) ->
            interval
                (minBound w (includes y))
                (maxBound x (includes y))

        ( Bounded w x, Bounded y z ) ->
            interval
                (minBound w y)
                (maxBound x z)
