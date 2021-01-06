% gvim:  fileencoding=utf8

/** <module> Some Predicates to somehow Mimic the Haskell QuickCheck Library
 *
 * @author José Martinez, October 2019, November 2020
 * @see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
 * @license proprietary
 *
 * This code cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
 * It is provided only for a pedagogical usage at Polytech Nantes.
 *
 * @version 1.1
 *
 * ***
 *
 * ALWAYS DOCUMENT AND TEST YOUR CODE!
 *
 * Documentation and tests are always much longer than your actual code...
 * (More than 1,000 lines could be removed here!)
 * Never neglect the usefulness of the latter with an untyped (or dynamically typed) language, especially when changing some line of code here and there.
 *
 * To see the documentation of this or another module:
 *    - start the interactive interpreter at the shell prompt:  "swipl";
 *    - run the HTTP documentation server: "doc_server(4000).";
 *    - load this module:  "consult('<MODULE>.pl').";
 *    - open (a new tab on) your Web browser (javascript enabled):  "doc_browser.".
 *    - "Et voilà!"
 *
 * You can have a look either only at the documentation (default view) or at the formatted code (click on the `(:-)` icon).
 * Actually, the latter is _strongly_ recommended for viewing this code.
 *
 * ***
 *
 * USAGE
 *
 * Unfortunately, this is not a translation of the original Haskell QuickCheck.
 * However, the idea is to reuse the methodology of:
 *
 *    * _generating_ random (input) parameters;
 *    * _running_ the predicates on them;
 *    * _checking_ whether the relationships between the input and output parameters hold.
 *
 * 
 * This module answers, still partially, the first point, i.e., it provides a way to generate random parameters.
 * (This corresponds to the "Arbitrary" class of functions.)
 * Of course, the last point is completely dependent on the predicate itself.
 * Then, mimicking QuickCheck is to be done with the following structure for Prolog tests.
 *
 * The structure of each test for a given predicate should resemble the following Prolog code:
 *
 * ==
 * test(post_condition_n) :-
 *    assertion( forall( ( between(1, 100, _)
 *                       , <generating the random input parameters>
 *                       , predicate(<parameters>)
 *                       )
 *                     , <checking the nth post-condition>
 *                     ) ).
 * ==
 *
 * It contains the three parts that have been introduced previously:
 *
 *    * The "forall" followed by "beetween(1, 100, _)" is there to run the test one hundred times.
 *      This is the inherited default value from QuickCheck.
 *      Depending on the time complexity of a predicate, it can be necessary to lower the number of times the predicate is tested.
 *      Conversely, depending on the complexity of the input space, it may be necessary to raise it in order to better sample the space.
 * 
 *    * Next, random parameters have to be generated in a specific way.
 *      Notice that all these predicates should be deterministic, which is the case for the predicates provided by this module.
 *      In effect, this first part of the "forall" predicate can fail without the test itself failing.
 *      This is the expected semantics of the universal quantifier.
 *      Subsequently, in the worst scenario, no valid data would be generated at all, hence the test would still be satisfied!
 *
 *    * Finally, when the predicate evaluation succeeds, one verifies that the given relationship actually holds.
 *
 * For instance, checking that the 'reverse/2' predicate is involutive, i.e., is its own inverse, can be checked as follows:
 *
 * ==
 * test(involutive) :-
 *    assertion( forall( ( between(1, 100, _)
 *                       , random_between(0, 50, N)               % Firstly, generating the random length of the random list,
 *                       , random_numbers_between(N, 10, 99, XS)) % and the random contents of the random list with random but distinguishable elements, most conveniently integers.
 *                       , reverse(XS, YS),                       % Next, applying the predicate.
 *                       )
 *                     , reverse(YS, XS)                          % Finally, checking that it is its own inverse.
 *                     ) ).
 * ==
 *
 * Contrary to QuickCheck, there is no automatic way to look for a minimal counter-example.
 * (What is missing is the set of "shrinking" strategies, e.g., for a non-empty list, splitting it into two halves or two interleaved sub-lists, or removing its head, etc.)
 * The programmer would have to modify the generation process in order to identify more clearly specific edge cases.
 *
 * For instance, should an implementation of the 'reverse/2' predicate fail, one could try to limit the size of the random lists, especially to zero and one.
 * Also, one could try to generate lists of identical elements.
 * Etc.
 *
 * Next, for debugging purposes, in case a (minimal) test fails, one can place a "write" before and/or after applying the predicate.
 *
 * For instance, the previous test could become:
 *
 * ==
 * test(involutive) :-
 *    assertion( forall( ( between(1, 100, _)
 *                       , random_between(0, 50, N)                % Firstly, generating the random length of the random list,
 *                       , random_numbers_between(N, 10, 99, XS))  % and the random contents of the random list with random but distinguishable elements, most conveniently integers.
 *                       , format("reverse(~w, _)\n", [XS]),       % Displaying the predicate before its execution.
 *                       , reverse(XS, YS),                        % Next, applying the predicate.
 *                       , format("reverse(~w, ~w)\n", [XS, YS]),  % Displaying the predicate after its execution.
 *                       )
 *                     , reverse(YS, XS)                           % Finally, checking that it is its own inverse.
 *                     ) ).
 * ==
 *
 * Should the information provided by the visualisation of the problematic parameters be insufficient, one can run the debugger (preferably the graphical one) on those values.
 *
 * ==
 * > gtrace(<predicate>).
 * > <predicate>(<parameters>).
 * ==
 *
 * Notice that when a predicate breaks and the programmer exits the execution with Ctrl-C, the stack is often left in an unstable state.
 * One has to leave and restart SWI-Prolog.
 * 
 * \todo Introduce a 'is_subset_of_between/4' that relates a subset containing number of elements between a minimum and a maximum from a set.
 *
 * \see [[Another (and Better) QuickCheck-inspired package][https://github.com/nicoabie/quickcheck]]
 *
 */

:- module(quickcheck, [ % Some Random Numbers Predicates

                        random_numbers_between/4
                      , distinct_random_numbers_between/4
                      , strictly_ordered_random_numbers_between/4

                        % Some Subset Predicates

                      , is_random_subset_of_exactly/3
                      , is_random_subset_of/2
                      , is_non_empty_random_subset/2
                      , is_random_subset_of_between/3
                      , is_random_subset_of_at_most/3
                      , is_non_empty_random_subset_of_at_most/3

                        % Some Sublist Predicates

                      , distinct_random_elements_in/3
                      , random_sumlist/3

                      , random_element/2
                      ]).

:- use_module(library(lists), [ sum_list/2 ]).
:- use_module(utilities, [ sublist/2
                         , deltas/2
                         , nths1/3
                         , all_different/1
                         , take/3
                         , is_subset_of/2
                         ]).

base_nths1(IS, ES, RS) :-
   findall(R, ( member(I, IS)
              , nth1(I, ES, R)
              ), RS).

:- begin_tests(nths1). % These tests have been postponed here since they use predicates from this module to be tested!

test(det) :-
   atom_chars(abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ, U),
   length(U, N_U),
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, N_U, N)
                      , strictly_ordered_random_numbers_between(N, 1, N_U, IS)
                      )
                    , aggregate_all(count, nths1(IS, U, _), 1)
                    ) ).

test(same_as_base) :-
   US = [ []
        , [a]
        , [a, b]
        , [a, b, c, d, e, f]
        , [a, b, c, d, e, f, a, b, c, d, e, f]
        , [a, b, c, d, e, f, a, b, c, d, e, f, b, c, d, e, f, a, b, c, d, e, f]
        , [a, b, c, d, e, f, a, b, c, d, e, f, b, c, d, e, f, a, b, c, d, e, f, a, b, c, d, e, f, a, b, c, d, e, f, b, c, d, e, f, a, b, c, d, e, f]
        ],
   assertion( forall( ( between(1, 100, _)
                      , random_element(US, U)
                      , length(U, N_U)
                      , random_between(0, N_U, N)
                      , strictly_ordered_random_numbers_between(N, 1, N_U, IS)
                      )
                    , ( base_nths1(IS, U, RS)
                      , nths1(IS, U, RS)
                      )
                    ) ).

test(required_length) :-
   US = [ []
        , [a]
        , [a, b]
        , [a, b, c, d, e, f]
        , [a, b, c, d, e, f, a, b, c, d, e, f]
        , [a, b, c, d, e, f, a, b, c, d, e, f, b, c, d, e, f, a, b, c, d, e, f]
        , [a, b, c, d, e, f, a, b, c, d, e, f, b, c, d, e, f, a, b, c, d, e, f, a, b, c, d, e, f, a, b, c, d, e, f, b, c, d, e, f, a, b, c, d, e, f]
        ],
   assertion( forall( ( between(1, 100, _)
                      , random_element(US, U)
                      , length(U, N_U)
                      , random_between(0, N_U, N)
                      , strictly_ordered_random_numbers_between(N, 1, N_U, IS)
                      , nths1(IS, U, RS)
                      )
                    , ( length(IS, N)
                      , length(RS, N)
                      )
                    ) ).

test(belong_to_the_list) :-
   US = [ []
        , [a]
        , [a, b]
        , [a, b, c, d, e, f]
        , [a, b, c, d, e, f, a, b, c, d, e, f]
        , [a, b, c, d, e, f, a, b, c, d, e, f, b, c, d, e, f, a, b, c, d, e, f]
        , [a, b, c, d, e, f, a, b, c, d, e, f, b, c, d, e, f, a, b, c, d, e, f, a, b, c, d, e, f, a, b, c, d, e, f, b, c, d, e, f, a, b, c, d, e, f]
        ],
   assertion( forall( ( between(1, 100, _)
                      , random_element(US, U)
                      , length(U, N_U)
                      , random_between(0, N_U, N)
                      , strictly_ordered_random_numbers_between(N, 1, N_U, IS)
                      , nths1(IS, U, RS)
                      )
                    , forall( member(X, RS)
                            , member(X, U)
                            )
                    ) ).

:- end_tests(nths1).

%! is_random_subset_of_exactly(-S:  list(term),  +U:  list(term),  +N:  integer) is det.
%
% Generation of random subset of a given set with the exact number of required and distinct elements.
%
% @arg S   A subset of the exactly N distinct elements from U.
% @arg U   A universe set.
% @arg N   The exact number of elements to randomly extract from U.
%
% @throws Precondition.   The set must contain at least N elements.
% @throws Precondition.   The universal set is actually a set.
%
% @throws Postcondition.   The generated subset has the number of demanded elements.
% @throws Postcondition.   The generated subset is actually a subset.
%
is_random_subset_of_exactly(S, U, N) :-
   assertion( ( length(U, N_U)
              , N_U >= N
              ) ),
   assertion( all_different(U) ),
   random_permutation(U, V),  % On permute aléatoirement la liste des éléments fournis,
   take(N, V, S).             % et on en récupère les premiers éléments.

:- begin_tests(is_random_subset_of_exactly).

test(det) :-
   atom_chars(abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ, U),
   length(U, N_U),
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, N_U, N)
                      )
                    , aggregate_all(count, is_random_subset_of_exactly(_, U, N), 1)
                    ) ).

test(demanded_number_of_elements) :-
   atom_chars(abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ, U),
   length(U, N_U),
   assertion( forall( between(1, 100, _)
                    , ( random_between(0, N_U, N)
                      , is_random_subset_of_exactly(S, U, N) % generation of a random subset
                      , length(S, N) % verification of the number of elements
                      )
                    ) ).

test(actually_subset) :-
   atom_chars(abcdefghijklmnopqrstuvwxyz, U),
   length(U, N_U),
   assertion( forall( ( random_between(0, N_U, N)
                      , is_random_subset_of_exactly(S, U, N) % generation of a random subset
                      )
                    , is_subset_of(S, U) % verification of the inclusion
                    ) ).

:- end_tests(is_random_subset_of_exactly).

%! is_random_subset_of(?S:  list(term), +U:  list(term)) is det.
%
% Generation of a single, random subset of a given set.
%
% @arg S   A subset of U.
% @arg U   A universe set.
%
% @throws Postcondition.   The generated subset is actually a subset.
%
is_random_subset_of(S, U) :-
   length(U, N_U),
   random_between(0, N_U, N),
   is_random_subset_of_exactly(S, U, N).

:- begin_tests(is_random_subset_of).

test(det) :-
   atom_chars(abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ, U),
   assertion( forall( between(1, 100, _)
                    , aggregate_all(count, is_random_subset_of(_, U), 1)
                    ) ).

test(actually_subsets) :-
   U = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z],
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(S, U) % generation of a random subset
                      )
                    , is_subset_of(S, U)          % verification of the inclusion
                    ) ).

:- end_tests(is_random_subset_of).

%! is_non_empty_random_subset(?S:  list(term), +U:  list(term)) is det.
%
% Generation of a single, random subset of a given set.
%
% @arg S   A subset of U.
% @arg U   A universe set.
%
% @throws Precondition.   The universe subset cannot be empty.
%
% @throws Postcondition.   The generated subset is actually a subset.
%
is_non_empty_random_subset(S, U) :-
   assertion( U \= [] ),
   length(U, N_U),
   random_between(1, N_U, N),
   is_random_subset_of_exactly(S, U, N).

:- begin_tests(is_non_empty_random_subset).

test(det) :-
   atom_chars(abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ, U),
   assertion( forall( between(1, 100, _)
                    , aggregate_all(count, is_non_empty_random_subset(_, U), 1)
                    ) ).

test(actually_subsets) :-
   U = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z],
   assertion( forall( ( between(1, 100, _)
                      , is_non_empty_random_subset(S, U) % generation of a random subset
                      )
                    , is_subset_of(S, U)                 % verification of the inclusion
                    ) ).

:- end_tests(is_non_empty_random_subset).

%! is_random_subset_of_between(?S:  list(term),  +U:  list(term),  +B: interval(integer, integer)) is det.
%
% Generation of all the subsets of a given set.
%
% @arg S                  A subset of the given universe.
% @arg U                  A universe set.
% @arg interval(LB, UB)   Interval specifying the minimal and maximum number of elements in the subset.
%
% @throws Precondition.   The lower bound cannot be negative.
% @throws Precondition.   The interval cannot be empty, i.e., LB >= UB.
% @throws Precondition.   The set must contain at least LB elements.
%
% @throws Postcondition.   The generated subset is actually a subset.
% @throws Postcondition.   The generated subset has the number of required elements.
%
is_random_subset_of_between(S, U, interval(LB, UB)) :-
   assertion( 0 =< LB ),
   assertion(      LB =< UB ),
   length(U, N_U),
   assertion( N_U >= LB),
   N_max is min(UB, N_U),
   random_between(LB, N_max, N),
   is_random_subset_of_exactly(S, U, N).

:- begin_tests(is_random_subset_of_between).

test(det) :-
   atom_chars(abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ, U),
   length(U, N_U),
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, N_U, N_min)
                      , random_between(N_min, N_U, N_max)
                      )
                    , aggregate_all(count, is_random_subset_of_between(_, U, interval(N_min, N_max)), 1)
                    ) ).

test(actually_subset) :-
   U = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z],
   length(U, N_U),
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, N_U, LB)
                      , random_between(LB, N_U, UB)
                      , is_random_subset_of_between(S, U, interval(LB, UB)) % generation of a random subset
                      )
                    , is_subset_of(S, U)                                    % verification of the inclusion
                    ) ).

test(required_elements) :-
   U = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z],
   length(U, N_U),
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, N_U, LB)
                      , random_between(LB, N_U, UB)
                      , is_random_subset_of_between(S, U, interval(LB, UB))
                      )
                    , ( length(S, N_S)
                      , LB =< N_S
                      ,       N_S =< UB
                      )
                    ) ).

:- end_tests(is_random_subset_of_between).

%! is_random_subset_of_at_most(?S:  list(term),  +U:  list(term),  +N:  integer) is det.
%
% Generation of all the subsets of a given set.
%
% @arg S   A subset of the given universe.
% @arg U   A universe set.
% @arg N   Maximal number of elements in the subset.
%
% @throws Precondition.   0 =< N
%
% @throws Postcondition.   The generated subset is actually a subset.
% @throws Postcondition.   Generated subsets have no more elements than required.
%
is_random_subset_of_at_most(S, U, N) :-
   assertion( 0 =< N ),
   length(U, N_U),
   N_max is min(N, N_U),
   random_between(0, N_max, M),
   is_random_subset_of_exactly(S, U, M).

:- begin_tests(is_random_subset_of_at_most).

test(det) :-
   U = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z],
   length(U, N_U),
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, N_U, N)
                      )
                    , aggregate_all(count, is_random_subset_of_at_most(_, U, N), 1)
                    ) ).

test(actually_subset) :-
   U = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z],
   length(U, N_U),
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, N_U, N)
                      , is_random_subset_of_at_most(S, U, N) % generation of a random subset
                      )
                    , is_subset_of(S, U)                     % verification of the inclusion
                    ) ).

test(maximal_cardinal) :-
   U = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z],
   length(U, N_U),
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, N_U, N)
                      , is_random_subset_of_at_most(S, U, N)
                      )
                    , ( length(S, N_S)
                      , N_S =< N
                      )
                    ) ).

:- end_tests(is_random_subset_of_at_most).

%! is_non_empty_random_subset_of_at_most(?S:  list(term),  +U:  list(term),  +N:  integer) is det.
%
% Generation of all the subsets of a given set.
%
% @arg S   A subset of the given universe.
% @arg U   A universe set.
% @arg N   Maximal number of elements in the subset.
%
% @throws Precondition.   N >= 1.  (Silent)
% @throws Precondition.   The universe cannot be empty.
%
% @throws Postcondition.   Generated subsets are actually subsets.
% @throws Postcondition.   Generated subsets have no more elements than required.
% @throws Postcondition.   Generated subsets are not empty.
%
% @bug.  Likewise 'is_random_subset_of_at_most/3'.
%
is_non_empty_random_subset_of_at_most(S, U, N) :-
   assertion( 0 < N ),
   length(U, N_U),
   assertion(     N =< N_U ),
   N_max is min(N, N_U),
   random_between(1, N_max, M),
   is_random_subset_of_exactly(S, U, M).

:- begin_tests(is_non_empty_random_subset_of_at_most).

test(actually_subset) :-
   U = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z],
   length(U, N_U),
   assertion( forall( ( between(1, 100, _)
                      , random_between(1, N_U, N)
                      , is_non_empty_random_subset_of_at_most(S, U, N)
                      )
                    , is_subset_of(S, U)
                    ) ).

test(maximal_cardinal) :-
   U = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z],
   length(U, N_U),
   assertion( forall( ( between(1, 100, _)
                      , between(1, N_U, N)
                      , is_non_empty_random_subset_of_at_most(S, U, N)
                      )
                    , ( length(S, N_S)
                      , N_S =< N
                      )
                    ) ).

test(not_empty) :-
   U = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z],
   length(U, N_U),
   assertion( forall( ( between(1, 100, _)
                      , between(1, N_U, N)
                      , is_non_empty_random_subset_of_at_most(S, U, N)
                      )
                    , S \= []
                    ) ).

:- end_tests(is_non_empty_random_subset_of_at_most).

%! random_sumlist(+N:  integer,  +M:  integer,  -VS:  list(integer)) is det.
%
% Distributes randomly a value over several entries, none being null.
%
% @arg N    Total number to distribute.
% @arg M    Number of bins on which to distribute N.
% @arg VS   Randomly distributed freedoms on the various bins, without exceeding their corresponding capacity.
%
% @throws Precondition.    N >= M >= 0.
%
% @throws Postcondition.   The number of distributed values is equal to M.
% @throws Postcondition.   The sum of the distributed values is equal to N.
% @throws Postcondition.   None of the distributed values is less than 1.
%
random_sumlist(0, 0, []) :-
   !.
random_sumlist(N, 1, [N]) :-
   assertion( N >= 1 ),
   !.
random_sumlist(N, M, VS) :-
   assertion( M > 1 ),
   assertion( N >= M ),
   divmod(M, 2, M1, R1),
   plus(M1, R1, M2),
   plus(M, F, N),
   plus(M1, F, N1_max),
   random_between(M1, N1_max, N1),
   plus(N1, N2, N),
   random_sumlist(N1, M1, VS1),
   random_sumlist(N2, M2, VS2),
   append(VS1, VS2, VS).

:- begin_tests(random_sumlist).

test(number_of_values) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(1, 1000, N)
                      , random_between(1, N, M)
                      , random_sumlist(N, M, VS)
                      )
                    , length(VS, M)
                    ) ).

test(sum_of_values) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(1, 1000, N)
                      , random_between(1, N, M)
                      , random_sumlist(N, M, VS)
                      )
                    , sum_list(VS, N)
                    ) ).

test(all_strictly_positive) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(1, 1000, N)
                      , random_between(1, N, M)
                      , random_sumlist(N, M, VS)
                      )
                    , forall( member(V, VS)
                            , V > 0
                            )
                    ) ).

:- end_tests(random_sumlist).

%! random_element(+L:  list(expression),  -X:  expression) is det.
%
% Random selection of an element in a non-empty list.
% Contrary to 'random_member/2', this predicate succeeds if X is already instantiated.
%
% @arg L   A list of elements.
% @arg X   One element from the list.
%
% @throws Precondition.    The list cannot be empty.  (Silent)
%
% @throws Postcondition.   The element actually belongs to the list.
%
random_element(L, X) :-
   var(X),
   length(L, N_L),
   random_between(1, N_L, I),
   nth1(I, L, X).
random_element(L, X) :-
   nonvar(X),
   member(X, L).

:- begin_tests(random_element).

test(random_element_actually_belongs_to_the_list) :-
   U = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z],
   assertion( forall( ( between(1, 100, _)
                      , random_element(U, X)
                      )
                    , member(X, U)
                    ) ).

:- end_tests(random_element).

%! random_numbers_between(+N:  int,  +L:  int,  +U:  int,  -RS:  list(int)) is det.
%
% Random selection of an element in a non-empty list.
% Contrary to 'random_member/2', this predicate succeeds if X is already instantiated.
%
% @arg N    Number of random numbers to generate.
% @arg L    Lower bound, inclusive, on the values to generate.
% @arg U    Upper bound, inclusive, on the values to generate.
% @arg XS   List of N distinct randoms numbers between L and U.
%
% @throws Precondition.    The number of numbers to generate is positive.  (Silent)
% @throws Precondition.    The number of numbers to generate cannot be larger than the integer interval.  (Silent)
%
% @throws Postcondition.   The list contains the required number of numbers.
% @throws Postcondition.   All the numbers belong to the interval.
%
random_numbers_between(N, L, U, XS) :-
   length(XS, N),
   maplist(random_between(L, U), XS).

:- begin_tests(random_numbers_between).

test(det) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 100, N)
                      , random_between(0, 100, L)
                      , random_between(L, 100, U)
                      )
                    , aggregate_all(count, random_numbers_between(N, L, U, _), 1)
                    ) ).

test(required_length) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 100, N)
                      , random_between(0, 100, L)
                      , random_between(L, 100, U)
                      , random_numbers_between(N, L, U, RS)
                      )
                    , length(RS, N)
                    ) ).

test(in_interval) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 100, N)
                      , random_between(0, 100, L)
                      , random_between(L, 100, U)
                      , random_numbers_between(N, L, U, RS)
                      )
                    , forall( member(R, RS)
                            , between(L, U, R)
                            )
                    ) ).

:- end_tests(random_numbers_between).

%! distinct_random_numbers_between(+N:  int,  +L:  int,  +U:  int,  -RS:  list(int)) is det.
%
% Random selection of an element in a non-empty list.
% Contrary to 'random_member/2', this predicate succeeds if X is already instantiated.
%
% @arg N    Number of random numbers to generate.
% @arg L    Lower bound, inclusive, on the values to generate.
% @arg U    Upper bound, inclusive, on the values to generate.
% @arg RS   List of N distinct randoms numbers between L and U.
%
% @throws Precondition.    The number of numbers to generate is positive.  (Silent)
% @throws Precondition.    The number of numbers to generate cannot be larger than the integer interval.  (Silent)
%
% @throws Postcondition.   The list contains the required number of numbers.
% @throws Postcondition.   All the numbers are different from each other.
% @throws Postcondition.   All the numbers belong to the interval.
%
% @bug  The running time is not bounded!
%       However, the code has been somewhat complexified in order to avoid worst cases, i.e., when N is in the top halp of (L - U + 1). 
%       This improvement is worth several order of magnitudes in running time!
%
distinct_random_numbers_between(N, L, U, RS) :-
   D is U - L + 1,
   N_sur_2 is div(N, 2),
   ( D =< N_sur_2
   -> positive_distinct_random_numbers_between(N, L, U, RS)
   ;  negative_distinct_random_numbers_between(N, L, U, RS)
   ).

positive_distinct_random_numbers_between(0, _, _, []).
positive_distinct_random_numbers_between(N, L, U, [R | RS]) :-
   0 =< N, I is U - L + 1, N =< I,
   plus(N_minus_1, 1, N),
   positive_distinct_random_numbers_between(N_minus_1, L, U, RS),
   repeat,
      random_between(L, U, R),
      \+ member(R, RS),
   !.

negative_distinct_random_numbers_between(N, L, U, RS) :-
   N_neg is U - L + 1 - N,
   positive_distinct_random_numbers_between(N_neg, L, U, NS),
   findall(V, between(L, U, V), VS),
   subtract(VS, NS, RS).

:- begin_tests(distinct_random_numbers_between).

test(required_length) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 10, N)
                      , random_between(0, 50, L)
                      , plus(L, N, L_plus_N)
                      , random_between(L_plus_N, 100, U)
                      , distinct_random_numbers_between(N, L, U, RS)
                      )
                    , length(RS, N)
                    ) ).

test(all_different) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 10, N)
                      , random_between(0, 50, L)
                      , plus(L, N, L_plus_N)
                      , random_between(L_plus_N, 100, U)
                      , distinct_random_numbers_between(N, L, U, RS)
                      )
                    , ( sort(RS, RS_sort)
                      , length(RS,      N)
                      , length(RS_sort, N)
                      )
                    ) ).

test(in_interval) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 10, N)
                      , random_between(0, 50, L)
                      , plus(L, N, L_plus_N)
                      , random_between(L_plus_N, 100, U)
                      , distinct_random_numbers_between(N, L, U, RS)
                      )
                    , forall( member(R, RS)
                            , between(L, U, R)
                            )
                    ) ).

:- end_tests(distinct_random_numbers_between).

%! strictly_ordered_random_numbers_between(+N:  integer,  +L:  integer,  +U:  integer,  -RS:  list(integer)) is det.
%
% Random selection of an element in a non-empty list.
% Contrary to 'random_member/2', this predicate succeeds if X is already instantiated.
%
% @arg N    Number of random numbers to generate.
% @arg L    Lower bound, inclusive, on the values to generate.
% @arg U    Upper bound, inclusive, on the values to generate.
% @arg RS   List of N distinct randoms numbers between L and U.
%
% @throws Precondition.    The number of numbers to generate is positive.  (Silent)
% @throws Precondition.    The number of numbers to generate cannot be larger than the integer interval.  (Silent)
%
% @throws Postcondition.   The list contains the required number of numbers.
% @throws Postcondition.   All the numbers are different from each other.
% @throws Postcondition.   All the numbers belong to the interval.
%
% @bug  The running time is not bounded!
%
strictly_ordered_random_numbers_between(N, L, U, RSO) :-
   distinct_random_numbers_between(N, L, U, RS),
   sort(RS, RSO).

:- begin_tests(strictly_ordered_random_numbers_between).

test(required_length) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 10, N)
                      , random_between(0, 50, L)
                      , plus(L, N, L_plus_N)
                      , random_between(L_plus_N, 100, U)
                      , strictly_ordered_random_numbers_between(N, L, U, RS)
                      )
                    , length(RS, N)
                    ) ).

test(all_different) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 10, N)
                      , random_between(0, 50, L)
                      , plus(L, N, L_plus_N)
                      , random_between(L_plus_N, 100, U)
                      , strictly_ordered_random_numbers_between(N, L, U, RS)
                      )
                    , ( sort(RS, RS_sort)
                      , length(RS,      N)
                      , length(RS_sort, N)
                      )
                    ) ).

test(in_interval) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 10, N)
                      , random_between(0, 50, L)
                      , plus(L, N, L_plus_N)
                      , random_between(L_plus_N, 100, U)
                      , strictly_ordered_random_numbers_between(N, L, U, RS)
                      )
                    , forall( member(R, RS)
                            , between(L, U, R)
                            )
                    ) ).

test(strictly_ordered) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 10, N)
                      , random_between(0, 50, L)
                      , plus(L, N, L_plus_N)
                      , random_between(L_plus_N, 100, U)
                      , strictly_ordered_random_numbers_between(N, L, U, RS)
                      )
                    , sort(RS, RS)
                    ) ).

:- end_tests(strictly_ordered_random_numbers_between).

%! distinct_random_elements_in(+N:  int,  +ES:  list(term),  -RS:  list(term)) is det.
%
% Random selection of a given number of elements in a (non-empty) list.
%
% @arg N    Number of random elements to select.
% @arg ES   List of elements in which to select some random ones.
% @arg RS   List of N distinct randoms elements from ES
%
% @throws Precondition.    The number of elements to randomly extract cannot be larger than the number of elements in the list.  (Silent)
% @throws Precondition.    The number of numbers to generate cannot be larger than the integer interval.  (Silent)
%
% @throws Postcondition.   The list contains the required number of elements.
% @throws Postcondition.   The random list is a sub-list of the given one.
%
distinct_random_elements_in(N, ES, RS) :-
   length(ES, M),
   N =< M,
   strictly_ordered_random_numbers_between(N, 1, M, NS),
   nths1(NS, ES, RS).

:- begin_tests(distinct_random_elements_in).

test(required_length) :-
   US = [ []
        , [a]
        , [a, b]
        , [a, b, c, d, e, f]
        , [a, b, c, d, e, f, a, b, c, d, e, f]
        , [a, b, c, d, e, f, a, b, c, d, e, f, b, c, d, e, f, a, b, c, d, e, f]
        , [a, b, c, d, e, f, a, b, c, d, e, f, b, c, d, e, f, a, b, c, d, e, f, a, b, c, d, e, f, a, b, c, d, e, f, b, c, d, e, f, a, b, c, d, e, f]
        ],
   assertion( forall( ( between(1, 100, _)
                      , random_element(US, U)
                      , length(U, N_U)
                      , random_between(0, N_U, N)
                      , distinct_random_elements_in(N, U, RS)
                      )
                    , length(RS, N)
                    ) ).

test(is_sublist) :-
   US = [ []
        , [a]
        , [a, b]
        , [a, b, c, d, e, f]
        , [a, b, c, d, e, f, a, b, c, d, e, f]
        , [a, b, c, d, e, f, a, b, c, d, e, f, b, c, d, e, f, a, b, c, d, e, f]
        , [a, b, c, d, e, f, a, b, c, d, e, f, b, c, d, e, f, a, b, c, d, e, f, a, b, c, d, e, f, a, b, c, d, e, f, b, c, d, e, f, a, b, c, d, e, f]
        ],
   assertion( forall( ( between(1, 100, _)
                      , random_element(US, U)
                      , length(U, N_U)
                      , random_between(0, N_U, N)
                      , distinct_random_elements_in(N, U, RS)
                      )
                    , sublist(RS, U)
                    ) ).

:- end_tests(distinct_random_elements_in).

% Comment the following line if you no longer need to test the predicates whenever the code is loaded.
%
% :- run_tests.

