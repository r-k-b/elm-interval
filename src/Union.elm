module Union exposing
    ( Union
    , fromInterval
    , fromIntervals
    , toIntervals
    , union
    , unionOfIntervals
    , intersection
    , intersectionWithInterval
    , subtract
    , subtractInterval
    , subtractUnions
    , unionToString
    )

{-| A set of strictly ordered, fully disjoint Intervals.


# Types

@docs Union

@docs fromInterval
@docs fromIntervals
@docs toIntervals

@docs union
@docs unionOfIntervals

@docs intersection
@docs intersectionWithInterval
@docs subtract
@docs subtractInterval
@docs subtractUnions
@docs unionToString

-}

import Interval
    exposing
        ( Interval
        , adjoins
        , hull
        , intersects
        , intervalToString
        , isEmpty
        , isLeftOpen
        , lowerBoundValue
        )


{-| A set of strictly non-overlapping Intervals.

Opaque type; do not export.

-}
type Union
    = Union (List Interval)


{-| The union of two Intervals. Will be disjoint (no intersecting intervals.)
E.g.:

  - [1, 2) ∪ [3, 4] = { [1, 2), [3, 4] }
  - [1, 2) ∪ [2, 3] = { [1, 3] }
  - [1, 2) ∪ (2, 3] = { [1, 2), (2, 3] }

-}
intervalUnion : Interval -> Interval -> Union
intervalUnion a b =
    union (fromInterval a) (fromInterval b)


{-| Construct a "union" from a single Interval.
-}
fromInterval : Interval -> Union
fromInterval a =
    case isEmpty a of
        True ->
            Union []

        False ->
            Union [ a ]


{-| Additively construct a union from an unordered list of arbitrary Intervals.
-}
fromIntervals : List Interval -> Union
fromIntervals intervals =
    fromIntervalsHelp (Union []) intervals


fromIntervalsHelp : Union -> List Interval -> Union
fromIntervalsHelp acc intervals =
    case intervals of
        [] ->
            acc

        next :: rest ->
            fromIntervalsHelp
                (union acc <| fromInterval next)
                rest


{-| If the union can be represented as a single Interval, return that interval.
-}
toInterval : Union -> Maybe Interval
toInterval (Union intervals) =
    case intervals of
        [] ->
            Just Interval.empty

        [ a ] ->
            Just a

        _ ->
            Nothing


{-| Merge two Unions.
E.g.:

  - { [1, 2) } ∪ { [3, 4] } = { [1, 2), [3, 4] }
  - { [1, 2) } ∪ { [2, 3] } = { [1, 3] }
  - { [1, 2) } ∪ { (2, 3] } = { [1, 2), (2, 3] }

-}
union : Union -> Union -> Union
union (Union a) (Union b) =
    Union <| unionHelp [] Nothing a b


empty : Union
empty =
    Union []


{-| Reduce a Union from the left.

    foldl (::) Union.empty todo_something == todo_somethingElse

-}
foldl : (Interval -> b -> b) -> b -> Union -> b
foldl func acc (Union intervals) =
    List.foldl func acc intervals


{-| Returns list of intervals
-}
toIntervals : Union -> List Interval
toIntervals (Union intervals) =
    intervals


{-| Construct a Union from the additive merger of two Intervals.
E.g.:

  - [1, 2) ∪ [3, 4] = { [1, 2), [3, 4] }

-}
unionOfIntervals : Interval -> Interval -> Union
unionOfIntervals a b =
    union (fromInterval a) (fromInterval b)


{-| All intervals must be sorted on their lower bounds, smallest first!

If the `List Interval`s contain any empty intervals, the correctness of the result is not guaranteed.

-}
unionHelp : List Interval -> Maybe Interval -> List Interval -> List Interval -> List Interval
unionHelp acc last a b =
    case last of
        Nothing ->
            let
                ( maybeNextInterval, restAs, restBs ) =
                    pickNextInterval a b
            in
            case maybeNextInterval of
                Nothing ->
                    acc

                Just _ ->
                    unionHelp acc maybeNextInterval restAs restBs

        Just theLast ->
            -- do we know theLast does not intersect or adjoin acc?
            case ( a, b ) of
                ( [], [] ) ->
                    List.append acc [ theLast ]

                ( [], nextB :: restBs ) ->
                    case nextB |> intersectsOrAdjoins theLast of
                        True ->
                            unionHelp acc (Just <| hull nextB theLast) [] restBs

                        False ->
                            let
                                nextAcc =
                                    List.append acc [ theLast ]
                            in
                            unionHelp nextAcc (Just nextB) [] restBs

                ( nextA :: restAs, [] ) ->
                    case nextA |> intersectsOrAdjoins theLast of
                        True ->
                            unionHelp acc (Just <| hull nextA theLast) restAs []

                        False ->
                            let
                                nextAcc =
                                    List.append acc [ theLast ]
                            in
                            unionHelp nextAcc (Just nextA) restAs []

                _ ->
                    let
                        ( maybeNextInterval, restAs, restBs ) =
                            pickNextInterval a b
                    in
                    case maybeNextInterval of
                        Nothing ->
                            -- shouldn't get here...
                            acc

                        Just nextInterval ->
                            case nextInterval |> intersectsOrAdjoins theLast of
                                True ->
                                    unionHelp acc (Just <| hull nextInterval theLast) restAs restBs

                                False ->
                                    let
                                        nextAcc =
                                            List.append acc [ theLast ]
                                    in
                                    unionHelp nextAcc (Just nextInterval) restAs restBs


intersectsOrAdjoins : Interval -> Interval -> Bool
intersectsOrAdjoins a b =
    (a |> intersects b) || (a |> adjoins b)


{-| Compare the lower bounds of the first two Intervals in each List, return
the interval with the lowest, along with the rest of the Lists.

If the `List Interval`s contain any empty intervals, the correctness of the
result is not guaranteed.

todo: replace all `List Interval` with `Union` in type signature

-}
pickNextInterval : List Interval -> List Interval -> ( Maybe Interval, List Interval, List Interval )
pickNextInterval a b =
    case ( a, b ) of
        ( [], [] ) ->
            ( Nothing, [], [] )

        ( [], nextB :: restBs ) ->
            ( Just nextB, [], restBs )

        ( nextA :: restAs, [] ) ->
            ( Just nextA, restAs, [] )

        ( nextA :: restAs, nextB :: restBs ) ->
            let
                nextA_cmp_nextB =
                    Maybe.map2 compare (lowerBoundValue nextA) (lowerBoundValue nextB)
            in
            case nextA_cmp_nextB of
                Nothing ->
                    {- Unions must never contain Empty intervals, and the
                       opaque type enforces it, but it's not clear how to
                       make that impossible and avoid this case via the type
                       system.
                    -}
                    ( Nothing, restAs, restBs )

                Just LT ->
                    ( Just nextA, restAs, b )

                Just EQ ->
                    case ( isLeftOpen nextA, isLeftOpen nextB ) of
                        ( False, _ ) ->
                            ( Just nextA, restAs, b )

                        ( _, False ) ->
                            ( Just nextB, a, restBs )

                        _ ->
                            ( Just nextA, restAs, b )

                Just GT ->
                    ( Just nextB, a, restBs )


{-| Subtract interval `b` from interval `a`, returning the parts of
`a` that did not intersect with `b`.

E.g.:

  - [1, 3) - (1, 2] = { {1}, (2, 3) }

-}
subtract : Interval -> Interval -> Union
subtract a b =
    fromIntervals <| Interval.subtract a b


{-| Subtract an interval from the given union, returning the contents of the
union that did not intersect with the interval.

E.g.:

  - { [1, 3), (5, 8] } - (1, 3) = { {1}, (5, 8] }

-}
subtractInterval : Interval -> Union -> Union
subtractInterval i u =
    subtractIntervalHelp empty i u


subtractIntervalHelp : Union -> Interval -> Union -> Union
subtractIntervalHelp acc i u =
    case u of
        Union [] ->
            acc

        Union (next :: rest) ->
            let
                newAcc =
                    union acc (subtract next i)
            in
            subtractIntervalHelp newAcc i (Union rest)


{-| Subtract union `b` from union `a`, returning the contents of `a` that
did not intersect with `b`.

E.g.:

  - { [1, 3), (5, 8] } - { (1, 3), [6, 7] } = { {1}, (5, 6), (7, 8] }

-}
subtractUnions : Union -> Union -> Union
subtractUnions (Union a) (Union b) =
    case ( a, b ) of
        ( _, [] ) ->
            Union a

        ( [], _ ) ->
            empty

        ( _, nextB :: restBs ) ->
            let
                diff =
                    subtractInterval nextB (Union a)
            in
            subtractUnions diff (Union restBs)


{-| Return the string representation of the given Union.
-}
unionToString : Union -> String
unionToString (Union intervals) =
    case intervals of
        [] ->
            "{}"

        _ ->
            let
                intervalString =
                    List.map intervalToString intervals
                        |> String.join ", "
            in
            "{ " ++ intervalString ++ " }"


{-| Return the intersection of an interval with a Union.
E.g.:

  - [1, 2) ∩ { [3, 5] } = {}
  - [1, 3) ∩ { [2, 5] } = { [2, 3) }
  - [2, 5] ∩ { [1, 3), [4, 6] } = { [2, 3), [4, 5] }

-}
intersectionWithInterval : Interval -> Union -> Union
intersectionWithInterval mainInterval theUnion =
    let
        func : Interval -> Union -> Union
        func eachInterval acc =
            union acc (fromInterval <| Interval.intersection eachInterval mainInterval)
    in
    foldl func empty theUnion


{-| Return the intersection of two Unions.
E.g.:

  - { [1, 2) } ∩ { [3, 5] } = {}
  - { [1, 3) } ∩ { [2, 5] } = { [2, 3) }
  - { [1, 3), [4, 6] } ∩ { [2, 5] } = { [2, 3), [4, 5] }

-}
intersection : Union -> Union -> Union
intersection a b =
    intersectionHelp (Union []) a b


intersectionHelp : Union -> Union -> Union -> Union
intersectionHelp acc (Union a) b =
    case a of
        [] ->
            acc

        nextA :: restAs ->
            let
                newAcc =
                    union acc (intersectionWithInterval nextA b)
            in
            intersectionHelp newAcc (Union restAs) b
