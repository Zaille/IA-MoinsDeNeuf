% gvim:  fileencoding=utf8

/** <module> Some Utility Predicates Absent from the Standard Libraries
 *
 * @author José Martinez, September 2018, October 15-16th, 2019
 * @see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
 * @license proprietary
 *
 * This code cannot be reproduced, copied, transmitted, etc., without the previous explicit authorisation of the author and/or the School.
 * It is provided only for a pedagogical usage at Polytech Nantes.
 *
 * @version 1.0
 *
 * === ALWAYS DOCUMENT AND TEST YOUR CODE!
 *
 * Documentation and tests are always much longer than your actual code...
 * (More than 1,000 lines could be removed here!)
 * Never neglect the usefulness of the latter with an untyped (or dynamically typed) language, especially when changing some line of code here and there.
 *
 * To see the documentation of this module:
 * - start the interactive interpreter at the shell prompt:  "swipl";
 * - run the HTTP documentation server: "doc_server(4000).";
 * - load this module:  "consult('utilities.pl').";
 * - open (a new tab on) your Web browser (javascript enabled):  "doc_browser.".
 * - "Et voilà!"
 *
 * You can have a look either only at the documentation (default view) or at the formatted code (click on the `(:-)` icon).
 * Actually, the latter is _strongly_ recommended for viewing this code.
 *
 * \todo Introduce a 'is_subset_of_between/4' that relates a subset containing number of elements between a minimum and a maximum from a set.
 *
 */

:- module(utilities, [ % ---+ Some Subset Predicates

                       is_subset_of/2
                     , is_non_empty_subset_of/2
                     , sublist/2

                       % ---+ Some Missing Utility Predicates

                     , multiply/3
                     , product_list/2

                       % ---+ Some Utility Predicates still to be Generalised

                     , all_different/1
                     , all_equal/1
                     , unique/2
                     , take/3
                     , take_at_most/3
                     , drop/3
                     , replicate/3
                     , interleave/3

                     , deltas/2
                     , nths1/3
                     
                     , argmax_list/2
                     , argmin_list/2

                     , rotate/2
                     , rotate_next/2
                     ]).

:- use_module(library(lists), [ sum_list/2
                              ]).

/**---+ Some Subset Predicates */


%! is_subset_of(+S:  list, +U:  list) is det.
%! is_subset_of(?S:  list, +U:  list) is nondet.
%! is_subset_of(+S:  list, ?U:  list) is det.
%
% Generation of all the subsets of a given set.
%
% @arg S   A subset of the universe set U (as a list).
% @arg U   A universe set (as a list).
%
% @throws Postcondition.   Generated subsets are actually subsets.
% @throws Postcondition.   The number of different subsets is the power of two of the cardinal of the universe set.
%
is_subset_of([], []).
is_subset_of([X | S], [X | U]) :-
   var(S),
   is_subset_of(S, U).
is_subset_of(S, [_ | U]) :-
   var(S),
   is_subset_of(S, U).
is_subset_of(S, U) :-
   nonvar(S),
   forall( member(X, S)
         , member(X, U)
         ).

:- begin_tests(is_subset_of).

test(actually_subsets) :-
   U = [a, b, c, d, e, f, g, h, i],
   assertion( forall( is_subset_of(S, U) % generation of subsets
                    , is_subset_of(S, U) % verification of the inclusion!
                    ) ).
   /*
   In case the previous assertion hurts your mind, you could use the following one.
   However, it will tend to blow up the available memory if you add some elements to U (maybe a single one!)...

   findall( S
          , is_subset_of(S, U) % generation of Ss
          , SS
          ),
   assertion( forall( member(S, SS)
                    , is_subset_of(S, U) % verification of inclusion
                    ) ).
   */

test(powers_of_two) :-
   assertion( forall( member(U, [ []
                                , [a]
                                , [a, b]
                                , [a, b, c]
                                , [a, b, c, d]
                                , [a, b, c, d, e]
                                ])
                    , ( findall(S, is_subset_of(S, U), SS)
                      , length(U, N_U)
                      , length(SS, N_SS)
                      , N_SS =:= 1 << N_U % i.e., 2^N
                      )
                    ) ).

:- end_tests(is_subset_of).

%! is_non_empty_subset_of(?S:  list, +U:  list) is nondet.
%
% Generation of all the non-empty subsets of a given set.
%
% @arg S   A subset of U.
% @arg U   A universe set.
%
% @throws Precondition.   The universe set cannot be empty.  (Silent)

% @throws Postcondition.   Generated subsets are actually (i) non empty (ii) subsets.
% @throws Postcondition.   The number of non empty subsets is the power of two of the cardinal of the set minus one.
%
is_non_empty_subset_of(S, U) :-
   is_subset_of(S, U),
   S \= [].

:- begin_tests(is_non_empty_subset_of).

test(actually_non_empty_subsets) :-
   U = [a, b, c, d, e, f, g, h, i],
   assertion( forall( is_non_empty_subset_of(S, U)   % generation of a subset
                    , ( is_non_empty_subset_of(S, U) % verification of the inclusion!
                      , length(S, N_S)
                      , N_S >= 1                     % verification of the cardinal
                      )
                    ) ).

test(powers_of_two) :-
   assertion( forall( member(U, [ []
                                , [a]
                                , [a, b]
                                , [a, b, c]
                                , [a, b, c, d]
                                , [a, b, c, d, e]
                                ])
                    , ( findall(S, is_non_empty_subset_of(S, U), SS)
                      , length(U, N_U)
                      , length(SS, N_SS_minus_1)
                      , plus(N_SS_minus_1, 1, N_SS)
                      , N_SS =:= 1 << N_U % i.e., 2^N
                      )
                    ) ).

:- end_tests(is_non_empty_subset_of).

/**---+ Some Missing Utility Predicates */

%! multiply(+A:  int, +B:  int, -C:  int) is det.
%! multiply(+A:  int, -B:  int, +C:  int) is semidet.
%! multiply(-A:  int, +B:  int, +C:  int) is semidet.
%
% Utility predicate to determine the missing value in the product of two numbers, or to verify the correctness of the product.
%
% @arg A   A number.
% @arg B   Another number
% @arg C   Their product
%
% @see   This is defined here since, contrary to 'plus/3', it is not a built-in predicate.
%
% @throws Postcondition.   When the product is known, the missing operand can be determined if, and only, if XXX
%
multiply(A, B, C) :-
   nonvar(A),
   nonvar(B),
   var(C),
   C is A * B.
multiply(A, B, C) :-
   nonvar(A),
   var(B),
   nonvar(C),
   A =\= 0,
   B is C / A.
multiply(A, B, C) :-
   var(A),
   nonvar(B),
   nonvar(C),
   B =\= 0,
   A is C / B.

:- begin_tests(multiply).

test(total_ab) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(-100, 100, A)
                      , random_between(-100, 100, B)
                      )
                    , multiply(A, B, _)
                    ) ).

test(correct_ab) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(-100, 100, A)
                      , random_between(-100, 100, B)
                      )
                    , ( multiply(A, B, C)
                      , C =:= A * B
                      )
                    ) ).

test(partial_ac) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(-100, 100, A)
                      , random_between(-100, 100, C)
                      )
                    , ( A =\= 0 -> multiply(A, _, C) ; true )
                    ) ).

test(correct_ac) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(-100, 100, A)
                      , A =\= 0
                      , random_between(-100, 100, C)
                      )
                    , ( multiply(A, B, C)
                      % , C =:= A * B
                      , E is abs(C - A * B)
                      , E =< 1e-6
                      )
                    ) ).

test(partial_bc) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(-100, 100, B)
                      , random_between(-100, 100, C)
                      )
                    , ( B =\= 0 -> multiply(_, B, C) ; true )
                    ) ).

test(correct_bc) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(-100, 100, B)
                      , B =\= 0
                      , random_between(-100, 100, C)
                      )
                    , ( multiply(A, B, C)
                      % , C =:= A * B
                      , E is abs(C - A * B)
                      , E =< 1e-6
                      )
                    ) ).

:- end_tests(multiply).

%! product_list(+L:  list(int), -P:  int) is det.
%
% Determines the iterated product of all the numbers of the list.
% By convention, the product of an empty list is one, i.e., the unit element of the corresponding binary operation.
%
% @arg L   A list of numbers.
% @arg P   Their product
%
% @see   Again, contrary to 'max_list/3', this predicate remains undefined in the base as well as list libraries.
%
% @throws  Postcondition.  The product of an empty list is the identity value of the product.
%
product_list(L, P) :-
   foldl(multiply, L, 1, P).

:- begin_tests(product_list).

test(total) :-
   assertion( forall( between(1, 100, _)
                    , ( random_between(0, 20, N)
                      , length(XS, N)
                      , maplist(random_between(10, 99), XS)
                      , product_list(XS, _)
                      )
                    ) ).

test(null_identity) :-
   assertion( product_list([], 1) ).

:- end_tests(product_list).

%!	sublist(-S: ?list, +L: list) is nondet.
%
% All the elements of the sublist appear in the list in the same order.
%
% @arg S   A sub-list of L.
% @arg L   A list of elements.
%
% This is reproduced as is from the HProlog compatibility library of SWI-Prolog.
%
% @bug  Caution, the number of sub-lists is exponential!
%       It is the power of two of the length of the list.
%
sublist(L, L).
sublist(Sub, [H|T]) :-
	'$sublist1'(T, H, Sub).

'$sublist1'(Sub, _, Sub).
'$sublist1'([H|T], _, Sub) :-
	'$sublist1'(T, H, Sub).
'$sublist1'([H|T], X, [X|Sub]) :-
	'$sublist1'(T, H, Sub).

%%% sublist([X | S], [X | L]) :-
%%%    sublist(S, L).
%%% sublist(S, [_ | L]) :-
%%%    sublist(S, L).
%%% sublist([], []).

:- begin_tests(sublist).

test(cardinal) :-
   assertion( forall( between(0, 100, _)
                    , ( random_between(0, 15, N_ES)
                      , length(ES, N_ES)
                      , maplist(random_between(10, 99), ES)
                      , aggregate_all(count, sublist(_, ES), N_SSS)
                      , pow(2, N_ES, N_SSS)
                      )
                    ) ).

:- end_tests(sublist).

%! all_different(+XS:  list(expression)) is det.
%
% Vérification de l'absence d'élément dupliqué dans une liste.
%
% @arg XS   Une liste d'éléments comparables par égalité.
%
% @throws Postcondition.   Le prédicat est satisfait si, et seulement si, tous les éléments sont distincts les uns des autres.
%
% __Time complexity.__  O(n^2) où n est la longueur de la liste.
% Il faut encore la multiplier par la complexité moyenne de comparaison par l'égalité des éléments de la liste.
%
all_different([]).          % dans une liste vide, tous les éléments sont bien différents les uns des autres
all_different([X | XS]) :-  % tandis que dans une liste d'au moins un élément
   \+ member(X, XS),        % il faut que le premier élément de la liste ne se retrouve pas dans la suite de la liste
   all_different(XS).       % et que la suite de la liste ne contienne pas de doublons non plus

:- begin_tests(all_different_ad_hoc).

test(un_cas_vrai) :-
   assertion( all_different([1, 2, 3, a, b, c, "bonjour", "le", "monde"]) ).

test(un_cas_faux) :-
   assertion( \+ all_different([1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4]) ).

:- end_tests(all_different_ad_hoc).

%! all_equal(+XS:  list(expression)) is det.
%
% Vérification de la présence d'éléments tous identiques dans une liste.
%
% @arg XS   Une liste d'éléments comparables par égalité.
%
% @throws Postcondition.   Le prédicat est satisfait si, et seulement si, tous les éléments sont égaux entre eux.
%
% __Time complexity.__  O(n) où n est la longueur de la liste.
% Il faut encore la multiplier par la complexité moyenne de comparaison par l'égalité des éléments de la liste.
%
all_equal([]).             % Dans une liste vide, tous les éléments sont bien égaux les uns des autres.
all_equal([_]).            % C'est également le cas dans une liste singleton.
all_equal([X, X | XS]) :-  % Enfin, c'est le cas si la liste contenant au moins deux éléments commence par un doublet
   all_equal([X | XS]).    % et continue de même.

:- begin_tests(all_equal_ad_hoc).

test(un_cas_faux) :-
   assertion( \+ all_equal([1, 2, 3, a, b, c, "bonjour", "le", "monde"]) ).

test(un_cas_vrai) :-
   assertion( all_equal([1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]) ).

:- end_tests(all_equal_ad_hoc).

%! unique(+XS:  list(expression), ?YS:  list(expression)) is det.
%
% Suppression des répétitions d'éléments dans une liste.
%
% @arg XS   Une liste d'éléments comparables par égalité.
% @arg YS   La liste des éléments distincts de XS.
%
% @throws Postcondition.   Tous les éléments de YS sont distincts les uns des autres.
% @throws Postcondition.   Tous les éléments de XS sont dans YS et réciproquement.
%
% __Time complexity.__  O(n.log n) où n est la longueur de la liste.
% Il faut encore la multiplier par la complexité moyenne de comparaison par l'égalité des éléments de la liste.
%
unique(XS, YS) :-
   list_to_set(XS, YS).
%% unique([],  []).           % Dans une liste vide, il n'y a rien à supprimer,
%% unique([X | XS], YS) :-    % tandis que dans une liste d'au moins un élément
%%    unique(XS, YS_sub),     % alors cette occurrence n'a pas à apparaître dans le résultat, construit à partir du reste de la liste seulement
%%    ( memberchk(X, YS_sub)  % si le premier élément apparaît aussi plus loin dans la liste
%%    *-> YS = YS_sub         % on l'ignore,
%%    ;   YS = [X | YS_sub]   % sinon, le premier élément est conservé.
%%    ).

:- begin_tests(unique).

test(total) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 100, N)
                      , length(XS, N)
                      , maplist(random_between(0, 9), XS)
                      )
                    , unique(XS, _)
                    ) ).

test(tous_differents) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 100, N)
                      , length(XS, N)
                      , maplist(random_between(0, 9), XS)
                      )
                    , ( unique(XS, YS)
                      , all_different(YS)
                      )
                    ) ).

test(tous_presents) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 100, N)
                      , length(XS, N)
                      , maplist(random_between(0, 9), XS)
                      )
                    , ( unique(XS, YS)
                      , forall( member(X, XS)
                              , member(X, YS)
                              )
                      )
                    ) ).

test(tous_presents_reciproque) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 100, N)
                      , length(XS, N)
                      , maplist(random_between(0, 9), XS)
                      )
                    , ( unique(XS, YS)
                      , forall( member(Y, YS)
                              , member(Y, XS)
                              )
                      )
                    ) ).

:- end_tests(unique).

%! take(+N:  nat, +XS: list, -PS:  list) is semidet.
%
% Retrieves or verifies that PS is the prefix of length N of XS.
%
% @arg N   Longueur du préfixe.
% @arg XS   Une liste d'éléments.
% @arg PS   Le préfixe de longueur N de XS, s'il existe (en longueur et en valeur)
%
% @bug Recopié depuis la bibliothèque 'xsb_lists' pour éviter d'avoir à la (télé)charger, sans recopier toute la notice!
%
% __Time complexity.__  O(N).
% Il faut encore la multiplier par la complexité moyenne de comparaison par l'égalité des éléments de la liste.
%
take(N, XS, PS) :-      % Un bel exemple de programmation non déterministe !
    length(PS, N),      % PS est un préfixe de taille N
    append(PS, _, XS).  % de XS, s'il apparaît en tête de ce dernier.

%! drop(+N:  nat, +XS: list, -PS:  list) is semidet.
%
% Retrieves or verifies that PS is the prefix of length N of XS.
%
% @arg N    Longueur du préfixe.
% @arg XS   Une liste d'éléments.
% @arg SS   Le suffixe après avoir ôté les N premiers élements de XS.
%
drop(N, XS, SS) :-
    length(PS, N),
    append(PS, SS, XS).

:- begin_tests(take_drop).

test(inverse) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 100, N_XS)
                      , length(XS, N_XS)
                      , maplist(random_between(10, 99), XS)
                      )
                    , ( random_between(0, N_XS, N)
                      , take(N, XS, PS)
                      , drop(N, XS, SS)
                      , append(PS, SS, XS)
                      )
                    ) ).

:- end_tests(take_drop).

%! take_at_most(+N:  nat, +XS: list, -PS:  list) is det.
%
% Retrieves or verifies that PS is the prefix of length at most N of XS.
%
% @arg N    Longueur maximale du préfixe.
% @arg XS   Une liste d'éléments.
% @arg PS   Le préfixe de longueur au plus N de XS, s'il existe (en longueur et en valeur)
%
% @throws Postcondition.   La longueur du résultat est inférieure ou égale à la longueur demandée si, et seulement si, la liste était plus courte.
%
take_at_most(N, XS, XS) :-
   length(XS, N_XS),
   N_XS =< N.
take_at_most(N, XS, PS) :-
   length(XS, N_XS),
   N_XS > N,
   take(N, XS, PS).

:- begin_tests(take_at_most).

test(total) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 100, N_XS)
                      , length(XS, N_XS)
                      , maplist(random_between(10, 99), XS)
                      , random_between(0, 100, N)
                      )
                    , take_at_most(N, XS, _)
                    ) ).

test(at_most) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 100, N_XS)
                      , length(XS, N_XS)
                      , maplist(random_between(10, 99), XS)
                      , random_between(0, 100, N)
                      , take_at_most(N, XS, PS)
                      )
                    , ( length(PS, N_PS)
                      , N_PS =< N
                      )
                    ) ).

test(shorter_if_shorter) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 100, N_XS)
                      , length(XS, N_XS)
                      , maplist(random_between(10, 99), XS)
                      , random_between(0, 100, N)
                      , take_at_most(N, XS, PS)
                      )
                    , ( length(PS, N_PS)
                      , ( N_PS < N -> N_XS < N
                        ; N_PS = N -> true
                        ; N_PS > N -> fail
                        )
                      )
                    ) ).

:- end_tests(take_at_most).

%! replicate(+X:  term,  +N:  nat,  +XS: list(term)) is semidet.
%
% Replicates the given number of time an element into a liste.
%
% @arg X   Un élélemnt
% @arg N   Le nombre de répétitions.
% @arg XS  La liste de longueur N ne contenant que la valeur X.
%
% @throws Postcondition.  La liste est de longueur demandée.
% @throws Postcondition.  La liste ne contient que la valeur donnée.
%
replicate(X, N, XS) :-
   length(XS, N),
   maplist(=(X), XS).

:- begin_tests(replicate).

test(longueur) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 100, X)
                      , random_between(0, 100, N)
                      )
                    , ( replicate(X, N, XS)
                      , length(XS, N)
                      )
                    ) ).

test(valeur_unique) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 100, X)
                      , random_between(0, 100, N)
                      )
                    , ( replicate(X, N, XS)
                      , forall( member(Y, XS)
                              , Y = X
                              )
                      )
                    ) ).

:- end_tests(replicate).

%! interleave(+XS:  list,  +N:  nat,  +XSS: list(list)) is det.
%
% Retrieves or verifies that PS is the prefix of length N of XS.
%
% @arg XS   Une liste d'éléments.
% @arg N    Le nombre de sous-listes à obtenir par interclassement.
% @arg XSS  Les sous-listes pouvant être interclassées.
%
% @throws Precondition.  Le nombre de sous-listes doit être strictement positif.
% @throws Precondition.  Le nombre de sous-listes doit être un diviseur de la longueur de la liste.
%
% @throws Postcondition.  Avec une sous-liste, cette dernière est aussi la liste initiale.
%
interleave(XS, N, XSS) :-
   assertion( N > 0 ),
   length(XS, N_XS),
   N_XS >= N,
   take(N, XS, YS),
   drop(N, XS, ZS),
   interleave(ZS, N, ZSS),
   maplist([A, B, C]>>(C = [A | B]), YS, ZSS, XSS).
interleave(XS, N, XSS) :-
   assertion( N > 0 ),
   length(XS, N_XS),
   N_XS < N,
   maplist([A, B]>>(B = [A]), XS, ZSS),
   plus(N_XS, M, N),
   replicate([], M, ESS),
   append(ZSS, ESS, XSS).

:- begin_tests(interleave).

test(total) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 100, N_XS)
                      , length(XS, N_XS)
                      , maplist(random_between(10, 99), XS)
                      , random_between(1, N_XS, N)
                      )
                    , interleave(XS, N, _)
                    ) ).

test(quasi_identite) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 100, N_XS)
                      , length(XS, N_XS)
                      , maplist(random_between(10, 99), XS)
                      )
                    , interleave(XS, 1, [XS])
                    ) ).

:- end_tests(interleave).

%! deltas(+VS:  list(int),  -DS:  list(int)) is det.
%
% Utility predicate to improve the running time of 'nths1/3'.
%
% @arg VS   List of integers.
% @arg DS   List of the differences between two consecutive integers of IS.
%
% @throws Postcondition.   The list DS contains as many elements as VS.
% @throws Postcondition.   A list VS can be reconstructed from DS (with 'scanl/4').
%
deltas([], []).
deltas([V_1 | VS], [V_1 | DS]) :-
   append(VS_init, [_], [V_1 | VS]),
   maplist([A, B, C]>>(plus(A, B, C)), DS, VS_init, VS).

:- begin_tests(deltas).

test(det) :-
   assertion( forall( between(1, 100, _)
                    , ( random_between(0, 1000, N)
                      , findall(V, ( between(1, N, _)
                                   , random_between(-30000, 30000, V)
                                   ), VS)
                      , aggregate_all(count, deltas(VS, _), 1)
                      )
                    ) ).

test(same_lengths) :-
   assertion( forall( between(1, 100, _)
                    , ( random_between(0, 1000, N)
                      , findall(V, ( between(1, N, _)
                                   , random_between(-30000, 30000, V)
                                   ), VS)
                      , deltas(VS, DS)
                      , length(VS, N_VS)
                      , length(DS, N_VS)
                      )
                    ) ).

test(invertible) :-
   assertion( forall( between(1, 100, _)
                    , ( random_between(0, 1000, N)
                      , findall(V, ( between(1, N, _)
                                   , random_between(-30000, 30000, V)
                                   ), VS)
                      , deltas(VS, DS)
                      , scanl(plus, DS, 0, [_ | VS])
                      )
                    ) ).

:- end_tests(deltas).

%! nths1(+IS:  list(int),  +ES:  list,  -RS:  list) is det.
%
% Efficient (linear) sub-sequence based on _non-decreasing_ indices.
%
% @arg IS   List of indices on the elements of the list, starting at one.
% @arg ES   List of elements in which to select some random ones.
% @arg RS   List of the indexed elements, in order.
%
% @throws Precondition.    The indices are strictly positive and less than the length of the list of elements.
%
% @throws Postcondition.   The resulting list contains as many elements as indices.
%
nths1(IS, ES, RS) :-
   deltas(IS, DS),
   delta_nths1(DS, ES, RS).

delta_nths1([], _, []).
delta_nths1([1 | DS], [E | ES], [E | RS]) :-
   delta_nths1(DS, ES, RS),
   !. % This is evil!
delta_nths1([D | DS], [_ | ES], RS) :-
   assertion( D > 1 ),
   plus(D_minus_1, 1, D),
   delta_nths1([D_minus_1 | DS], ES, RS).

%! argmax_list(-M:  value,  +AVS:  list(tuple(key, value))) is nondet.
%
% @arg V_argmax   Value associated to one of the maximal keys.
% @arg AVS        List of tuples consisting of an argument, the key, and its associated value
%
% @throws Precondition.    The list must not be empty. (Silent)
%
% @throws Postcondition.   The found value exists in the second components of the list.
% @throws Postcondition.   All the keys are not larger than the one associated to the value.
%
argmax_list(V_argmax, AVS) :-
   maplist([(A, _), A]>>true, AVS, AS),
   max_list(AS, A_max),
   member((A_max, V_argmax), AVS).

:- begin_tests(argmax_list).

test(partial_if_empty) :-
   assertion( \+ argmax_list(_, []) ).

test(total_if_not_empty) :-
   assertion( forall( between(1, 100, _)
                    , ( random_between(1, 100, N)
                      , length(XS, N)
                      , maplist([(A, V)]>>( random_between(0, 100, A)
                                          , random_between(1000, 5000, V)
                                          ), XS)
                      , argmax_list(_, XS)
                      )
                    ) ).

test(value_exists) :-
   assertion( forall( between(1, 100, _)
                    , ( random_between(1, 100, N)
                      , length(XS, N)
                      , maplist([(A, V)]>>( random_between(0, 100, A)
                                          , random_between(1000, 5000, V)
                                          ), XS)
                      , argmax_list(V_max, XS)
                      , member((_, V_max), XS)
                      )
                    ) ).

test(no_greater_argument) :-
   assertion( forall( between(1, 100, _)
                    , ( random_between(1, 100, N)
                      , length(XS, N)
                      , maplist([(A, V)]>>( random_between(0, 100, A)
                                          , random_between(1000, 5000, V)
                                          ), XS)
                      , argmax_list(V_max, XS)
                      , member((K_max, V_max), XS)
                      , forall( member((K, _), XS)
                              , K =< K_max
                              )
                      )
                    ) ).

:- end_tests(argmax_list).

%! argmin_list(-M:  value,  +AVS:  list(tuple(key, value))) is nondet.
%
% @arg V_argmin   Value associated to one of the minimal keys.
% @arg AVS        List of tuples consisting of an argument, the key, and its associated value
%
% @throws Precondition.    The list must not be empty. (Silent)
%
% @throws Postcondition.   The found value exists in the second components of the list.
% @throws Postcondition.   All the keys are not lesser than the one associated to the value.
%
argmin_list(V_argmin, AVS) :-
   maplist([(A, _), A]>>true, AVS, AS),
   min_list(AS, A_min),
   member((A_min, V_argmin), AVS).

:- begin_tests(argmin_list).

test(partial_if_empty) :-
   assertion( \+ argmin_list(_, []) ).

test(total_if_not_empty) :-
   assertion( forall( between(1, 100, _)
                    , ( random_between(1, 100, N)
                      , length(XS, N)
                      , maplist([(A, V)]>>( random_between(0, 100, A)
                                          , random_between(1000, 5000, V)
                                          ), XS)
                      , argmin_list(_, XS)
                      )
                    ) ).

test(value_exists) :-
   assertion( forall( between(1, 100, _)
                    , ( random_between(1, 100, N)
                      , length(XS, N)
                      , maplist([(A, V)]>>( random_between(0, 100, A)
                                          , random_between(1000, 5000, V)
                                          ), XS)
                      , argmin_list(V_min, XS)
                      , member((_, V_min), XS)
                      )
                    ) ).

test(no_lesser_argument) :-
   assertion( forall( between(1, 100, _)
                    , ( random_between(1, 100, N)
                      , length(XS, N)
                      , maplist([(A, V)]>>( random_between(0, 100, A)
                                          , random_between(1000, 5000, V)
                                          ), XS)
                      , argmin_list(V_min, XS)
                      , member((K_min, V_min), XS)
                      , forall( member((K, _), XS)
                              , K >= K_min
                              )
                      )
                    ) ).

:- end_tests(argmin_list).

%! rotate(+XS: [term], +YS: [term]) is semidet.
%! rotate(+XS: [term], -YS: [term]) is multi.
%! rotate(-XS: [term], +YS: [term]) is multi.
%! rotate(-XS: [term], -YS: [term]) is multi.
%
% Ensemble des rotations de listes.
%
% @arg XS   Value associated to one of the minimal keys.
% @arg YS   List of tuples consisting of an argument, the key, and its associated value
%
% @throws Precondition.   Both lists have the same length.  (Silent)
%
% @throws Postcondition.   Each list is its own rotation, i.e., this relation is reflexive.
% @throws Postcondition.   The relation is also symmetrical.
% @throws Postcondition.   The number of rotations is equal to the length of the list, and one for the empty list as a special case.
%
% @bug   Caution, the version where the first list is a variable whereas the second one is instantiated leads to returning the expected answers but, unfortunately, leads subsequently to a stack overflow...
% @bug   Caution, should both lists be variables then the result set is infinite, as expected, though it is well explored, i.e., nothing is forgotten.
%
rotate([], []).
rotate(XS, YS) :-
   length(XS, N),
   length(YS, N),
   append(PS, SS, XS),
   SS \= [],
   append(SS, PS, YS).

:- begin_tests(rotate).

test(defined_1) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 50, N)
                      , length(XS, N)
                      , maplist(random_between(10, 99), XS)
                      )
                    , rotate(XS, _)
                    ) ).

test(defined_2) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 50, N)
                      , length(YS, N)
                      , maplist(random_between(10, 99), YS)
                      )
                    , rotate(_, YS)
                    ) ).

test(reflexive) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 50, N)
                      , length(XS, N)
                      , maplist(random_between(10, 99), XS)
                      )
                    , rotate(XS, XS)
                    ) ).

test(symmetrical_1) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 50, N)
                      , length(XS, N)
                      , maplist(random_between(10, 99), XS)
                      )
                    , ( rotate(XS, XS_rot)
                      , rotate(XS_rot, XS)
                      )
                    ) ).

test(symmetrical_2) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 50, N)
                      , length(YS, N)
                      , maplist(random_between(10, 99), YS)
                      )
                    , ( rotate(XS_rot, YS)
                      , rotate(XS_rot, YS_back)
                      , YS_back = YS
                      )
                    ) ).

test(number_empty_1) :-
   assertion( aggregate_all(count, rotate([], _), 1) ).

/* Stack overflow

test(number_empty_2) :-
   assertion( aggregate_all(count, rotate(_, []), 1) ).
*/

test(number_non_empty_1) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(1, 50, N)
                      , length(XS, N)
                      , maplist(random_between(10, 99), XS)
                      )
                    , aggregate_all(count, rotate(XS, _), N)
                    ) ).

/* Stack overflow

test(number_non_empty_2) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(1, 50, N)
                      , length(YS, N)
                      , maplist(random_between(10, 99), YS)
                      )
                    , aggregate_all(count, rotate(_, YS), N)
                    ) ).
*/

:- end_tests(rotate).

%! rotate_next(+XS: [term], +YS: [term]) is semidet.
%! rotate_next(+XS: [term], -YS: [term]) is det.
%! rotate_next(-XS: [term], +YS: [term]) is det.
%! rotate_next(-XS: [term], -YS: [term]) is multi.
%
% Next right rotation.
%
% @arg XS   A list of elements
% @arg YS   A list which is a right rotation of the first one, i.e., the first element of the former is the last of this one
%
% @throws Precondition.   Both lists have the same length.  (Silent)
%
% @throws Postcondition.   This is a special case of 'rotate/2'.
% @throws Postcondition.   The relation is symmetrical.
%
% @bug   Caution, should both lists be variables then the result set is infinite, as expected, as well as completely explored.
%
rotate_next([], []).
rotate_next([X | XS], YS) :-
   append(XS, [X], YS).

:- begin_tests(rotate_next).

test(defined_1) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 50, N)
                      , length(XS, N)
                      , maplist(random_between(10, 99), XS)
                      )
                    , rotate_next(XS, _)
                    ) ).

test(defined_2) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 50, N)
                      , length(XS, N)
                      , maplist(random_between(10, 99), XS)
                      )
                    , rotate_next(_, XS)
                    ) ).

test(special_1) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 50, N)
                      , length(XS, N)
                      , maplist(random_between(10, 99), XS)
                      , rotate_next(XS, YS)
                      )
                    , rotate(XS, YS)
                    ) ).

test(special_2) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 50, N)
                      , length(YS, N)
                      , maplist(random_between(10, 99), YS)
                      , rotate_next(XS, YS)
                      )
                    , rotate(XS, YS)
                    ) ).

test(symmetrical_1) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 50, N)
                      , length(XS, N)
                      , maplist(random_between(10, 99), XS)
                      )
                    , ( rotate_next(XS, XS_next)
                      , rotate_next(XS_back, XS_next)
                      , XS_back = XS
                      )
                    ) ).

test(symmetrical_2) :-
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 50, N)
                      , length(YS, N)
                      , maplist(random_between(10, 99), YS)
                      )
                    , ( rotate_next(XS, YS)
                      , rotate_next(XS, YS_back)
                      , YS_back = YS
                      )
                    ) ).

:- end_tests(rotate_next).

% Comment the following line if you no longer need to test the predicates whenever the code is loaded.

% :- run_tests. % Currently, 'nths1/3' tests are actually conducted in the module 'quickcheck.pl'.

