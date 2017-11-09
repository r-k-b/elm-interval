module Union
    exposing
        ( fromInterval
        , fromIntervals
        , Union
        , union
        , unionOfIntervals
        , unionToString
        , subtract
        )

{-| A set of strictly non-overlapping Intervals.
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
    case isEmpty (a) of
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

  - { [1, 2) } ∪ [3, 4] = { [1, 2), [3, 4] }
  - { [1, 2) } ∪ [2, 3] = { [1, 3] }
  - { [1, 2) } ∪ (2, 3] = { [1, 2), (2, 3] }

-}
union : Union -> Union -> Union
union (Union a) (Union b) =
    Union <| unionHelp [] Nothing a b


unionOfIntervals : Interval -> Interval -> Union
unionOfIntervals a b =
    union (fromInterval a) (fromInterval b)


{-| All intervals must be sorted on their lower bounds, smallest first!
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

                    Just nextInterval ->
                        unionHelp acc maybeNextInterval restAs restBs

        Just theLast ->
            -- do we know theLast does not intersect or adjoin acc?
            case ( a, b ) of
                ( [], [] ) ->
                    List.append acc [ theLast ]

                ( [], nextB :: restBs ) ->
                    case (nextB |> intersectsOrAdjoins theLast) of
                        True ->
                            unionHelp acc (Just <| hull nextB theLast) [] restBs

                        False ->
                            let
                                nextAcc =
                                    List.append acc [ theLast ]
                            in
                                unionHelp nextAcc (Just nextB) [] restBs

                ( nextA :: restAs, [] ) ->
                    case (nextA |> intersectsOrAdjoins theLast) of
                        True ->
                            unionHelp acc (Just <| hull nextA theLast) restAs []

                        False ->
                            let
                                nextAcc =
                                    List.append acc [ theLast ]
                            in
                                unionHelp nextAcc (Just nextA) restAs []

                ( _, _ ) ->
                    let
                        ( maybeNextInterval, restAs, restBs ) =
                            pickNextInterval a b
                    in
                        case maybeNextInterval of
                            Nothing ->
                                -- shouldn't get here...
                                acc

                            Just nextInterval ->
                                case (nextInterval |> intersectsOrAdjoins theLast) of
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

todo: replace all `List Interval` with `Union` in type sig

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
                case (nextA_cmp_nextB) of
                    Nothing ->
                        Debug.crash "Unions must never contain Empty intervals!"

                    Just LT ->
                        ( Just nextA, restAs, b )

                    Just EQ ->
                        case ( isLeftOpen nextA, isLeftOpen nextB ) of
                            ( False, _ ) ->
                                ( Just nextA, restAs, b )

                            ( _, False ) ->
                                ( Just nextB, a, restBs )

                            ( _, _ ) ->
                                ( Just nextA, restAs, b )

                    Just GT ->
                        ( Just nextB, a, restBs )


{-| Subtract union `b` from union `a`, returning the contents of `a` that did not intersect with `b`.
-}
subtract : Union -> Union -> Union
subtract a b =
    Debug.crash "todo"


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


{-| Return the intersection of two Unions.
E.g.:

  - { [1, 2) } ∩ { [3, 5] } = {}
  - { [1, 3) } ∩ { [2, 5] } = { [2, 3) }
  - { [1, 3), [4, 6] } ∩ { [2, 5] } = { [2, 3), [4, 5] }

-}
intersect : Union -> Union -> Union
intersect a b =
    Debug.crash "todo"
