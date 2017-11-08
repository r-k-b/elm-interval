module Interval
    exposing
        ( degenerate
        , empty
        , excludes
        , hull
        , includes
        , intersection
        , intersects
        , intersectsPoint
        , interval
        , intervalToString
        , leftBounded
        , rightBounded
        , unbounded
        )

{-| A representation of numeric intervals (also known as *ranges*.)


# Constructors

@docs interval
@docs degenerate
@docs empty
@docs leftBounded
@docs rightBounded
@docs unbounded


# Endpoint (Bound) constructors

@docs excludes
@docs includes


# Operations on Intervals

@docs hull
@docs intersection
@docs intervalToString


# Tests on Intervals

@docs intersects
@docs intersectsPoint


# Related reading

  - [Interval](https://en.wikipedia.org/wiki/Interval_(mathematics))
  - [Interval tree])<https://en.wikipedia.org/wiki/Interval_tree>)
  - [Allen's interval algebra](https://en.wikipedia.org/wiki/Allen%27s_interval_algebra)

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


{-| Return the outer minimum of two Bounds.

    minOuterBound (includes 1) (excludes 1) == (includes 1)

    minOuterBound (includes 1) (excludes 0) == (excludes 0)

-}
minOuterBound : Bound -> Bound -> Bound
minOuterBound a b =
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
                        andInclusives a b


{-| Return the outer maximum of two Bounds.

    maxOuterBound (includes 1) (excludes 1) == (includes 1)

    maxOuterBound (includes 1) (excludes 2) == (excludes 2)

-}
maxOuterBound : Bound -> Bound -> Bound
maxOuterBound a b =
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
                        andInclusives a b


{-| Return the inner minimum of two Bounds.

    minOuterBound (includes 1) (excludes 1) == (excludes 1)

    minOuterBound (includes 0) (excludes 1) == (includes 0)

-}
minInnerBound : Bound -> Bound -> Bound
minInnerBound a b =
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
                        andExclusives a b


{-| Return the inner maximum of two Bounds.

    maxInnerBound (includes 1) (excludes 1) == (excludes 1)

    maxInnerBound (includes 0) (excludes 1) == (includes 0)

-}
maxInnerBound : Bound -> Bound -> Bound
maxInnerBound a b =
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
                        andExclusives a b


{-| If either Bound is Exclusive, return that. Else, both are Inclusive; return the first.
-}
andInclusives : Bound -> Bound -> Bound
andInclusives a b =
    case ( a, b ) of
        ( Inclusive _, Inclusive _ ) ->
            a

        ( Exclusive _, _ ) ->
            a

        ( _, Exclusive _ ) ->
            b


{-| If either Bound is Inclusive, return that. Else, both are Exclusive; return the first.
-}
andExclusives : Bound -> Bound -> Bound
andExclusives a b =
    case ( a, b ) of
        ( Exclusive _, Exclusive _ ) ->
            a

        ( Inclusive _, _ ) ->
            a

        ( _, Inclusive _ ) ->
            b


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
                (maxOuterBound (includes w) y)
                (minOuterBound (includes w) z)

        ( Bounded w x, Degenerate y ) ->
            interval
                (maxOuterBound w (includes y))
                (minOuterBound x (includes y))

        ( Bounded w x, Bounded y z ) ->
            interval
                (maxOuterBound w y)
                (minOuterBound x z)


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
                (minOuterBound (includes w) y)
                (maxOuterBound (includes w) z)

        ( Bounded w x, Degenerate y ) ->
            interval
                (minOuterBound w (includes y))
                (maxOuterBound x (includes y))

        ( Bounded w x, Bounded y z ) ->
            interval
                (minOuterBound w y)
                (maxOuterBound x z)


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

        ( Degenerate w, Bounded y z ) ->
            (intersection a b) == a

        ( Bounded w x, Degenerate y ) ->
            (intersection a b) == b

        ( Bounded w x, Bounded y z ) ->
            (intersection a b) /= empty


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

        Bounded w x ->
            intersection a (degenerate n) == (degenerate n)
