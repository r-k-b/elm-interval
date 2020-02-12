
# elm-interval

![.github/workflows/make.yml](https://github.com/r-k-b/elm-interval/workflows/.github/workflows/make.yml/badge.svg)

Intervals for Elm. Handles Unions, Intersections, and intervals with any 
combination of open / closed bounds.


# What is an interval?

An open interval does not include its endpoints, and is indicated with 
parentheses. For example, `(0,1)` means greater than 0 and less than 1. 
A closed interval is an interval which includes all its limit points, and is 
denoted with square brackets. For example, `[0,1]` means greater than or equal
to 0 and less than or equal to 1. A half-open interval includes only one of
its endpoints, and is denoted by mixing the notations for open and closed
intervals. `(0,1]` means greater than 0 and less than or equal to 1, while 
`[0,1)` means greater than or equal to 0 and less than 1.

A degenerate interval is any set consisting of a single real number. Some
authors include the empty set in this definition. A real interval that is
neither empty nor degenerate is said to be proper, and has infinitely many
elements.

An interval is said to be left-bounded or right-bounded if there is some real
number that is, respectively, smaller than or larger than all its elements.
An interval is said to be bounded if it is both left- and right-bounded; and
is said to be unbounded otherwise. Intervals that are bounded at only one end
are said to be half-bounded. The empty set is bounded, and the set of all
reals is the only interval that is unbounded at both ends. Bounded intervals
are also commonly known as finite intervals.

An interval is said to be left-open if and only if it contains no minimum 
(an element that is smaller than all other elements); right-open if it
contains no maximum; and open if it has both properties. The interval 
`[0,1) = {x | 0 ≤ x < 1}`, for example, is left-closed and right-open.
The empty set and the set of all reals are open intervals, while the set of 
non-negative reals, for example, is a right-open but not left-open interval.
The open intervals coincide with the open sets of the real line in its
standard topology.

An interval is said to be left-closed if it has a minimum element, right-closed
if it has a maximum, and simply closed if it has both. These definitions are
usually extended to include the empty set and to the (left- or right-)
unbounded intervals, so that the closed intervals coincide with closed sets in
that topology.


# todos

In roughly descending order of priority:

- [ ] add examples of real use cases to the readme (potentially replacing the "What is an interval?" filler)
- [ ] sort out the build / test systems (using make or npm?)
- [ ] implement Interval Trees
