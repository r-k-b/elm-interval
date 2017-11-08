module Union
    exposing
        ( fromInterval
        , Union
        , union
        )

{-| A set of strictly non-overlapping Intervals.
-}

import Interval exposing (Interval)


{-| A set of strictly non-overlapping Intervals.

Opaque type; do not export.

-}
type Union
    = Union (List Interval)


{-| The union of two Intervals.
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
    Union [ a ]


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
union a b =
    Debug.crash "todo"


{-| Return the intersection of two Unions.
E.g.:

  - { [1, 2) } ∩ { [3, 5] } = {}
  - { [1, 3) } ∩ { [2, 5] } = { [2, 3) }
  - { [1, 3), [4, 6] } ∩ { [2, 5] } = { [2, 3), [4, 5] }

-}
intersect : Union -> Union -> Union
intersect a b =
    Debug.crash "todo"
