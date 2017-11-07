module Interval exposing (interval, intervalToString, intersection, includes, excludes, empty, unbounded)

{-| A representation of numeric intervals (also known as *ranges*.)

See also [Wikipedia on intervals][WP].
[WP]: <https://en.wikipedia.org/wiki/Interval_(mathematics)>

@docs interval
@docs intervalToString
@docs intersection
@docs includes
@docs excludes
@docs empty
@docs unbounded

<https://en.wikipedia.org/wiki/Allen%27s_interval_algebra> ?
<https://en.wikipedia.org/wiki/Interval_tree> ?

-}


{-| An interval over the extended reals.
`Bounded x y` will always satisfy x <= y.

(Opaque type; do not export)

-}
type Interval
    = Bounded Bound Bound
    | Degenerate Float
    | LeftBounded Bound
    | RightBounded Bound
    | Unbounded
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


{-| An unbounded Interval.
-}
unbounded : Interval
unbounded =
    Unbounded


{-| Constructs an `Interval` from two Bounds. Argument order does not matter.
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
                case ( isInfinite t, isInfinite u, t < u ) of
                    ( False, False, False ) ->
                        Bounded j i

                    ( False, False, True ) ->
                        Bounded i j

                    ( False, True, False ) ->
                        -- u == -∞
                        RightBounded i

                    ( False, True, True ) ->
                        -- u == +∞
                        LeftBounded i

                    ( True, False, False ) ->
                        -- t == +∞
                        LeftBounded j

                    ( True, False, True ) ->
                        -- t == -∞
                        RightBounded j

                    ( True, True, _ ) ->
                        -- t == +∞, u == -∞
                        -- or
                        -- t == -∞, u == +∞
                        Unbounded


{-| Return a String representation of an Interval.
-}
intervalToString : Interval -> String
intervalToString interval =
    case interval of
        Unbounded ->
            "[-∞, ∞]"

        Empty ->
            "{}"

        Degenerate n ->
            "{" ++ toString n ++ "}"

        LeftBounded b ->
            case b of
                Exclusive n ->
                    "(" ++ toString n ++ ", ∞]"

                Inclusive n ->
                    "[" ++ toString n ++ ", ∞]"

        RightBounded b ->
            case b of
                Exclusive n ->
                    "[-∞, " ++ toString n ++ ")"

                Inclusive n ->
                    "[-∞, " ++ toString n ++ "]"

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


{-| The intersection of two intervals. If the intervals overlap, this is the common part. If not, this is the empty interval.
-}
intersection : Interval -> Interval -> Interval
intersection a b =
    case ( a, b ) of
        ( Empty, _ ) ->
            Empty

        ( _, Empty ) ->
            Empty

        ( _, Unbounded ) ->
            a

        ( Unbounded, _ ) ->
            b

        ( Degenerate x, Degenerate y ) ->
            case (x == y) of
                True ->
                    Degenerate x

                False ->
                    Empty

        ( Degenerate x, LeftBounded b ) ->
            let
                contained =
                    case b of
                        Inclusive w ->
                            w <= x

                        Exclusive w ->
                            w < x
            in
                case contained of
                    True ->
                        Degenerate x

                    False ->
                        Empty

        ( Degenerate x, RightBounded b ) ->
            let
                contained =
                    case b of
                        Inclusive y ->
                            x <= y

                        Exclusive y ->
                            x < y
            in
                case contained of
                    True ->
                        Degenerate x

                    False ->
                        Empty

        ( Degenerate x, Bounded lower upper ) ->
            let
                contained =
                    case ( lower, upper ) of
                        ( Inclusive w, Inclusive y ) ->
                            w <= x && x <= y

                        ( Inclusive w, Exclusive y ) ->
                            w <= x && x < y

                        ( Exclusive w, Inclusive y ) ->
                            w < x && x <= y

                        ( Exclusive w, Exclusive y ) ->
                            w < x && x < y
            in
                case contained of
                    True ->
                        Degenerate x

                    False ->
                        Empty

        ( LeftBounded b, Degenerate y ) ->
            let
                contained =
                    case b of
                        Inclusive x ->
                            x <= y

                        Exclusive x ->
                            x < y
            in
                case contained of
                    True ->
                        Degenerate y

                    False ->
                        Empty

        ( RightBounded b, Degenerate y ) ->
            let
                contained =
                    case b of
                        Inclusive z ->
                            y <= z

                        Exclusive z ->
                            y < z
            in
                case contained of
                    True ->
                        Degenerate y

                    False ->
                        Empty

        ( Bounded lower upper, Degenerate y ) ->
            let
                contained =
                    case ( lower, upper ) of
                        ( Inclusive x, Inclusive z ) ->
                            x <= y && y <= z

                        ( Inclusive x, Exclusive z ) ->
                            x <= y && y < z

                        ( Exclusive x, Inclusive z ) ->
                            x < y && y <= z

                        ( Exclusive x, Exclusive z ) ->
                            x < y && y < z
            in
                case contained of
                    True ->
                        Degenerate y

                    False ->
                        Empty

        ( LeftBounded lowerA, LeftBounded lowerB ) ->
            let
                a =
                    boundValue lowerA

                b =
                    boundValue lowerB
            in
                case (a < b) of
                    True ->
                        LeftBounded lowerA

                    False ->
                        Debug.crash "todo"

        ( RightBounded upperA, RightBounded upperB ) ->
            Debug.crash "todo"

        ( LeftBounded lowerA, RightBounded upperB ) ->
            Debug.crash "todo"

        ( RightBounded upperA, LeftBounded lowerB ) ->
            Debug.crash "todo"

        ( Bounded lowerA upperA, LeftBounded lowerB ) ->
            Debug.crash "todo"

        ( Bounded lowerA upperA, RightBounded upperB ) ->
            Debug.crash "todo"

        ( LeftBounded lowerA, Bounded lowerB upperB ) ->
            Debug.crash "todo"

        ( RightBounded upperA, Bounded lowerB upperB ) ->
            Debug.crash "todo"

        ( Bounded lowerA upperA, Bounded lowerB upperB ) ->
            case lowerA of
                Inclusive la ->
                    case lowerB of
                        Inclusive lb ->
                            case upperA of
                                Inclusive ua ->
                                    case upperB of
                                        Inclusive ub ->
                                            -- [la, ua] ∩ [lb, ub]
                                            Debug.crash "todo"

                                        Exclusive ub ->
                                            -- [la, ua] ∩ [lb, ub)
                                            Debug.crash "todo"

                                Exclusive ua ->
                                    case upperB of
                                        Inclusive ub ->
                                            -- [la, ua) ∩ [lb, ub]
                                            Debug.crash "todo"

                                        Exclusive ub ->
                                            -- [la, ua) ∩ [lb, ub)
                                            Debug.crash "todo"

                        Exclusive lb ->
                            case upperA of
                                Inclusive ua ->
                                    case upperB of
                                        Inclusive ub ->
                                            -- [la, ua] ∩ (lb, ub]
                                            Debug.crash "todo"

                                        Exclusive ub ->
                                            -- [la, ua] ∩ (lb, ub)
                                            Debug.crash "todo"

                                Exclusive ua ->
                                    case upperB of
                                        Inclusive ub ->
                                            -- [la, ua) ∩ (lb, ub]
                                            Debug.crash "todo"

                                        Exclusive ub ->
                                            -- [la, ua) ∩ (lb, ub)
                                            Debug.crash "todo"

                Exclusive la ->
                    case lowerB of
                        Inclusive lb ->
                            case upperA of
                                Inclusive ua ->
                                    case upperB of
                                        Inclusive ub ->
                                            -- (la, ua] ∩ [lb, ub]
                                            Debug.crash "todo"

                                        Exclusive ub ->
                                            -- (la, ua] ∩ [lb, ub)
                                            Debug.crash "todo"

                                Exclusive ua ->
                                    case upperB of
                                        Inclusive ub ->
                                            -- (la, ua) ∩ [lb, ub]
                                            Debug.crash "todo"

                                        Exclusive ub ->
                                            -- (la, ua) ∩ [lb, ub)
                                            Debug.crash "todo"

                        Exclusive lb ->
                            case upperA of
                                Inclusive ua ->
                                    case upperB of
                                        Inclusive ub ->
                                            -- (la, ua] ∩ (lb, ub]
                                            Debug.crash "todo"

                                        Exclusive ub ->
                                            -- (la, ua] ∩ (lb, ub)
                                            Debug.crash "todo"

                                Exclusive ua ->
                                    case upperB of
                                        Inclusive ub ->
                                            -- (la, ua) ∩ (lb, ub]
                                            Debug.crash "todo"

                                        Exclusive ub ->
                                            -- (la, ua) ∩ (lb, ub)
                                            Debug.crash "todo"



{-

   {-| The convex hull of two intervals. This is similar to union in that it includes all the points of the component intervals,
   and for non-overlapping intervals, the points between them.
   -}
   hull : Interval number -> Interval number -> Interval number
   hull (Interval a) (Interval b) =
      Interval { min = minEndPoint a.min b.min, max = maxEndPoint a.max b.max }

-}
