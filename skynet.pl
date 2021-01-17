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
                        choisir_pile_min_points/3,
                        recup_pioche_opti/4
                      ]).

:- use_module(utilities, [
                         argmin_list/2,
                         argmax_list/2
                         ]).



%! min_points_piles(+T3: sommets, -P) is det.
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


%! choisir_pile_min_points(+T3: sommets, +B, -P) is det.
%
% Retourne la pile qui n'est pas celle ou la plus petite carte se trouve.
%
% @arg T3       Les sommets des trois piles sur la table
% @arg B        La liste des piles disponibles
% @arg P        La pile
%
% @throws Precondition.    /
%
% @throws Postcondition.   Retourne les de la plus petite carte.
%
choisir_pile_min_points(_, [pile_1], pile_1) .
choisir_pile_min_points(_, [pile_2], pile_2) .
choisir_pile_min_points(sommets(T1, T2, _), [pile_1,pile_2], P) :-
   min_points_pile(T1, V1),
   min_points_pile(T2, V2),
   ( V1 =< V2 -> P = pile_2
   ; V1 > V2 -> P = pile_1 ),
   !.


%! min_points_piles(+T3: sommets, +N_P, -C: { carte, 'pioche' }) is det.
%
% Retourne les points de la plus petite carte présente sur les deux piles.
%
% @arg T3       Les sommets des trois piles sur la table
% @arg N_P      Le nombre de cartes dans la pioche
% @arg C        La carte ramassée, éventuellement dans la pioche
%
% @throws Precondition.    /
%
% @throws Postcondition.   Retourne les de la plus petite carte.
%
defaut_pioche(CS, 0, C) :-
   findall((V, K), (member(K, CS), carte(K, V, _)), VCS),              % après avoir calculé la valeur de chaque carte visible,
   % maplist([K, (V, K)]>>carte(K, V, _), CS, VCS),
   argmin_list(C, VCS),                                                % elle sélectionne une seule de celles permettant de prendre un minimum de points,
   !.                                                                  % et une seule.
defaut_pioche(CS, N_P, C) :-
   N_P > 0,                                                            % Si la pioche n'est pas vide
   findall((V, K), (member(K, CS), carte(K, V, _)), VCS),              % après avoir calculé la valeur de chaque carte visible,
   % maplist([K, (V, K)]>>carte(K, V, _), CS, VCS),
   argmin_list(C_min, VCS),                                            % es sélectionné une,
   !,                                                                  % et une seule, de celles
   carte(C_min, V_min, _),                                             % de valeur minimale,
   ( V_min =< 7 -> C = C_min                                           % si la valeur de cette dernière est inférieure ou égale à sept, alors on la garde,
   ; V_min >  7 -> C = pioche                                          % sinon on pioche car la probabilité de prendre plus petit a dépassé la moyenne.
   ),
   !.

%! min_points_piles(+T3: sommets, +M: [carte], +N_P: atom, -C: { carte, 'pioche' }) is det.
%
% Retourne les points de la plus petite carte présente sur les deux piles.
%
% @arg T3       Les sommets des trois piles sur la table
% @arg M        La main
% @arg N_P      Le nombre de cartes dans la pioche
% @arg C        La carte ramassée, éventuellement dans la pioche
%
% @throws Precondition.    /
%
% @throws Postcondition.   Retourne les de la plus petite carte.
%
recup_pioche_opti(T, M, N_P, C) :-
   length(M, L),
   findall((P, CT), (member(CT, T), combinaison([CT|M], CB), cartes_combinaison(CS, CB), length(CS, NCS), L > NCS, points_cartes(CS, P)), CSS),
   length(CSS, CSSL),
   (( CSSL >= 1, argmax_list(C, CSS) )
   ; ( CSSL == 0, defaut_pioche(T, N_P, C) )),
   !.


