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
                        recup_pioche_opti/4,
                        check_piles_for_combi/3,
                        get_max_score_combi/6,
                        remove_elements_liste/3,
                        remove_combi_liste/3,
                        defausse_pioche_opti/2
                      ]).

:- use_module(utilities, [
                         argmin_list/2,
                         argmax_list/2
                         ]).

:- use_module(moins_de_neuf, [
                         piles_defausse_possible/2
                         ]).


%%%%%%%%%%%%%%%%%%%%%%
%      Défausse      %
%%%%%%%%%%%%%%%%%%%%%%


%! defausse_pioche_opti(+M: sommets, -C_max) is det.
%
% Déterminer le nombre de points maximals qu'il est possible de défausser à partir du jeu en main.
%
% @arg M            La main
% @arg C_max        Nombre de points maximals pouvant être défausser
%
% @throws Precondition.    /
%
% @throws Postcondition.   Une défausse possible avec le nombre de points total.
%
defausse_pioche_opti(M, C_max) :-
   combinaisons(M, CSS),
   findall((P, CS), (member(CS, CSS), cartes_combinaison(KS, CS), points_cartes(KS, P)), KPS),
   findall((P1, [C]), (member(C, M), points_carte(C, P1)), CPS),
   append(KPS, CPS, LC),
   argmax_list(C_max, LC),
   !.

%! remove_combi_liste(+CB, +LCS, -LCS2).
%
% Supprime une combinaison d'une liste de combinaisons.
%
% @arg CB       La combinaison à supprimer
% @arg LCS      La liste de combinaisons
% @arg LCS2     La liste de combinaisons avec la combinaison supprimée
%
% @throws Precondition.    \
%
% @throws Postcondition.   Liste identique ou avec une combinaison en moins.
%
remove_combi_liste(_, [], []).
remove_combi_liste(CB, [CS|LCS], [LCS1|LCS2]) :-
   remove_elements_liste(CB, CS, LCS1),
   remove_combi_liste(CB, LCS, LCS2).

%! remove_elements_liste(+CB, +M, -M2).
%
% Supprime une combinaison de la main.
%
% @arg CB       La combinaison à supprimer
% @arg M        La main
% @arg M2       La main avec la combinaison supprimée
%
% @throws Precondition.    \
%
% @throws Postcondition.   Main identique ou avec une combinaison en moins.
%
remove_elements_liste([], M, M).
remove_elements_liste([CS|CB], M, M2) :-
    remove_element_liste(CS, M, M1),
    remove_elements_liste(CB, M1, M2).

%! remove_element_liste(+CS, +M, -M1).
%
% Supprime une carte de la main.
%
% @arg CS       La carte à supprimer
% @arg M        La main
% @arg M1       La main avec la carte supprimée
%
% @throws Precondition.    \
%
% @throws Postcondition.   Main identique ou avec une carte en moins.
%
remove_element_liste(_, [], []).
remove_element_liste(CS, [CS|M], M1) :-
    remove_element_liste(CS, M, M1).
remove_element_liste(CS, [C|M], [C|M1]) :-
    C \= CS,
    remove_element_liste(CS, M, M1).

%! get_max_score_combi(+CSS, -S2, -P2, -CB2, -C2, +T3).
%
% Permet de choisir la pile de défausse en anticipant la carte qui est la plus utile en fonction de notre jeu.
%
% @arg CSS       Liste d'informations sur les combinaisons possibles avec les cartes en sommet de pile
% @arg S2        Score max des combinaisons possibles avec les cartes en sommet de pile
% @arg P2        La pile ou se défausser
% @arg CB2       La combinaison la plus intéressante à créer avec les cartes en sommet de pile
% @arg C2        La carte à piocher pour créer la combinaison
% @arg T3        Les sommets des trois piles sur la table
%
% @throws Precondition.    \
%
% @throws Postcondition.   Actions précises de défausse et de pioche
%
get_max_score_combi([], 0, P, [], _, T3) :-
    piles_defausse_possible(T3, PS),
    !,
    choisir_pile_min_points(T3, PS, P).
get_max_score_combi([(P1,CB,C,S)|[]], S, P2, CB, C, _) :-
    ( ( P1 == pile_1, P2 = pile_2 ) ; ( P2 = pile_1 ) ),
    !.
get_max_score_combi([(P1,CB1,C1,S1)|CSS], S2, P2, CB2, C2, _) :-
    get_max_score_combi(CSS, S3, P3, CB3, C3, _),
    ((
        S1 > S3,
        S2 = S1,
        C2 = C1,
        CB2 = CB1,
        ( ( P1 == pile_1, P2 = pile_2 ) ; ( P2 = pile_1 ) )
    ) ; (
        S2 = S3,
        P2 = P3,
        C2 = C3,
        CB2 = CB3
    )),
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

%! check_pile(+T, +CS, +N, -CSS).
%
% Liste toutes les combinaisons possibles avec une combinaison en paramètre et les cartes en sommet de la pile.
%
% @arg T          Le sommet d'une pile sur la table
% @arg CS         Une combinaison
% @arg N          Le nom de la pile
% @arg CSS        Liste des combinaisons possibles avec chacune des cartes en sommet de la pile
%
% @throws Precondition.    \
%
% @throws Postcondition.   Les combinaisons proposées sont plus intéressantes que celles possibles initialement avec notre jeu
%
check_pile(T, CS, N, CSS) :-
   length(CS, L),
   findall((N, CCS, C, P1), (member(C, T), combinaison([C|CS], CB), cartes_combinaison(CCS, CB), length(CCS, NCS), NCS is L + 1, points_cartes(CS, P1)), CSS).

%! check_piles_for_combi(+T3, +CS, -TLS).
%
% Liste toutes les combinaisons possibles avec la main en paramètre et les cartes en sommet de pile.
%
% @arg T3         Les sommets des trois piles sur la table
% @arg CS         Une combinaison
% @arg TLS        Liste des combinaisons possibles avec chacune des cartes en sommet de pile
%
% @throws Precondition.    \
%
% @throws Postcondition.   Les combinaisons proposées sont plus intéressantes que celles possibles initialement avec notre jeu
%
check_piles_for_combi(sommets(T1,T2,_), M, CSS) :-
   check_pile_for_combi(T1, M, pile_1, CSS1),
   check_pile_for_combi(T2, M, pile_2, CSS2),
   append(CSS1, CSS2, CSS),
   !.

%! check_pile_for_combi(+T, +M, +N, -CSS).
%
% Liste toutes les combinaisons possibles avec notre main en paramètre et les cartes en sommet de la pile.
%
% @arg T          Le sommet d'une pile sur la table
% @arg M          Notre main
% @arg N          Le nom de la pile
% @arg CSS        Liste des combinaisons possibles avec chacune des cartes en sommet de la pile
%
% @throws Precondition.    \
%
% @throws Postcondition.   Les combinaisons proposées sont plus intéressantes que celles possibles initialement avec notre jeu
%
check_pile_for_combi(T, M, N, CSS) :-
   length(M, L),
   findall((N, CS, C, P), (member(C, T), combinaisons([C|M], CB), CB \= [], select_combi_max(CB, C, L, CS), cartes_combinaison(CB_max, CS), points_cartes(CB_max, P)), CSS),
   !.

%! select_combi_max(+CB, +C, +L, -CS_max).
%
% Récupère la combinaison d'une liste de combinaisons avec le score le plus élevé
%
% @arg CB          Liste de combinaisons
% @arg C           Une carte, pour vérifier si elle est présente dans les combinaisons
% @arg L           Taille de la main
% @arg CS_MAX      Le score maximale parmi toutes les combinaisons
%
% @throws Precondition.    \
%
% @throws Postcondition.   Un score est retourné si la Carte apparait dans l'une des combinaison et si sa taille est inférieur
%                          au nombre de carte dans la main.
%
select_combi_max(CB, C, L, CS_max) :-
   findall((P, CS), (member(CS, CB), cartes_combinaison(KS, CS), member(C,KS), length(KS, NCS), L > NCS, points_cartes(KS, P)), KPS),
   argmax_list(CS_max, KPS),
   !.

%%%%%%%%%%%%%%%%%%%%%%
%       Pioche       %
%%%%%%%%%%%%%%%%%%%%%%


%! recup_pioche_opti(+T3: sommets, +M: [carte], +N_P: atom, -C: { carte, 'pioche' }) is det.
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
   (( CSSL > 0, argmax_list(C, CSS) )
   ; ( CSSL == 0, defaut_pioche(T, N_P, C) )),
   !.

%! defaut_pioche(+T3: sommets, +N_P, -C: { carte, 'pioche' }) is det.
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


%%%%%%%%%%%%%%%%%%%%%%
%      Annonce       %
%%%%%%%%%%%%%%%%%%%%%%

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


