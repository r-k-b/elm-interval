module Bound exposing
    ( Bound(..)
    , minOuter, maxOuter, minInner, maxInner
    , invert
    , value, isOpen
    )

{-| A representation of numeric bounds.


# Types

@docs Bound


# Operations on Bounds

@docs minOuter, maxOuter, minInner, maxInner
@docs invert


# Tests on Bounds

@docs value, isOpen

-}


{-| Represents an upper or lower, closed or open endpoint of an Interval.
This encompasses the "endpoints" of unbounded intervals when the bound value
is either of the `Infinity` values in the floating point spec.

Opaque type.

-}
type Bound
    = Inclusive Float
    | Exclusive Float


{-| The value stored in the bound.
-}
value : Bound -> Float
value b =
    case b of
        Inclusive n ->
            n

        Exclusive n ->
            n


{-| Hold the bound value steady, but invert the open/closed property.
-}
invert : Bound -> Bound
invert b =
    case b of
        Inclusive n ->
            Exclusive n

        Exclusive n ->
            Inclusive n


{-| Whether the bound is open (exclusive) or closed (inclusive).
-}
isOpen : Bound -> Bool
isOpen b =
    case b of
        Inclusive _ ->
            False

        Exclusive _ ->
            True


{-| Return the outer minimum of two Bounds.

    minOuter (Inclusive 1) (Exclusive 1) == Inclusive 1

    minOuter (Inclusive 1) (Exclusive 0) == Exclusive 0

-}
minOuter : Bound -> Bound -> Bound
minOuter a b =
    let
        x : Float
        x =
            value a

        y : Float
        y =
            value b
    in
    case x < y of
        True ->
            a

        False ->
            case y < x of
                True ->
                    b

                False ->
                    -- x == y
                    andInclusives a b


{-| Return the outer maximum of two Bounds.

    maxOuter (Inclusive 1) (Exclusive 1) == Inclusive 1

    maxOuter (Inclusive 1) (Exclusive 2) == Exclusive 2

-}
maxOuter : Bound -> Bound -> Bound
maxOuter a b =
    let
        x : Float
        x =
            value a

        y : Float
        y =
            value b
    in
    case x < y of
        True ->
            b

        False ->
            case y < x of
                True ->
                    a

                False ->
                    -- x == y
                    andInclusives a b


{-| Return the inner minimum of two Bounds.

    minInner (Inclusive 1) (Exclusive 1) == Exclusive 1

    minInner (Inclusive 0) (Exclusive 1) == Inclusive 0

-}
minInner : Bound -> Bound -> Bound
minInner a b =
    let
        x : Float
        x =
            value a

        y : Float
        y =
            value b
    in
    case x < y of
        True ->
            a

        False ->
            case y < x of
                True ->
                    b

                False ->
                    -- x == y
                    andExclusives a b


{-| Return the inner maximum of two Bounds.

    maxInnerBound (Inclusive 1) (Exclusive 1) == Exclusive 1

    maxInnerBound (Inclusive 0) (Exclusive 1) == Inclusive 0

-}
maxInner : Bound -> Bound -> Bound
maxInner a b =
    let
        x : Float
        x =
            value a

        y : Float
        y =
            value b
    in
    case x < y of
        True ->
            b

        False ->
            case y < x of
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
