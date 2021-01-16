% gvim:  fileencoding=utf8

/** <module> Our super AI
 *
 * @author Lucas Hervouet & Étienne Lécrivain
 * @license proprietary
 *
 * @version 1.0
 *
 */

:- module(skynet, [
                        min_points_piles/2,
                        choisir_pile_min_points/3
                      ]).

:- use_module(utilities, [
                         argmin_list/2
                         ]).



%! min_points_piles(+T3: sommets, +B, -P) is det.
%
% Retourne les points de la plus petite carte présente sur les deux piles.
%
% @arg T3       Les sommets des trois piles sur la table
% @arg P        Les points de la plus petite carte
%
% @throws Precondition.    /
%
% @throws Postcondition.   Retourne les de la plus petite carte.
%
min_points_piles(sommets(T1, T2, _), P) :-
   append(T1, T2, CS),                                                 % on concat les deux piles
   findall((V, K), (member(K, CS), carte(K, V, _)), VCS),
   argmin_list(C, VCS),                                                % on récupère la carte avec le minimum de points,
   points_carte(C, P),
   !.

min_points_pile([], 1000) .
min_points_pile(T, P) :-
  findall((V, K), (member(K, T), carte(K, V, _)), VCS),
  argmin_list(C, VCS),                                                % on récupère la carte avec le minimum de points,
  points_carte(C, P),
  !.


choisir_pile_min_points(_, [pile_1], pile_1) .
choisir_pile_min_points(_, [pile_2], pile_2) .
choisir_pile_min_points(sommets(T1, T2, _), [pile_1,pile_2], P) :-
   min_points_pile(T1, V1),
   min_points_pile(T2, V2),
   ( V1 =< V2 -> P = pile_2
   ; V1 > V2 -> P = pile_1 ),
   !.

