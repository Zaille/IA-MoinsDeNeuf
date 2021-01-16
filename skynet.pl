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
                        min_points_piles/2
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
