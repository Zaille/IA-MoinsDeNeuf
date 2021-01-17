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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% DÉFAUSSE
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
   combinaisons(M, CSS),                                                                            % Récupération de toutes les combinaisons possiblent avec les cartes dans notre main
   findall((P, CS), (member(CS, CSS), cartes_combinaison(KS, CS), points_cartes(KS, P)), KPS),      % Calcul des points de chaque combinaison possible
   findall((P1, [C]), (member(C, M), points_carte(C, P1)), CPS),                                    % Calcul des points de chaque carte de notre main
   append(KPS, CPS, LC),                                                                            % Fusion des deux listes de points
   argmax_list(C_max, LC),                                                                          % Récuperation de la combinaison / carte avec le maximum de points
   !.

%! remove_combi_liste(+CB, +LCS, -LCS2) is det.
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
   remove_elements_liste(CB, CS, LCS1),             % Appelle de la fonction pour supprimer la combinaison si elle correspond
   remove_combi_liste(CB, LCS, LCS2),               % Appelle récursif pour traiter la combinaison suivante
   !.

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
    remove_element_liste(CS, M, M1),            % Appelle de la fonction pour supprimer la combinaison de la main
    remove_elements_liste(CB, M1, M2),          % Appelle récursif pour traiter la combinaison suivante
    !.

%! remove_element_liste(+CS, +M, -M1) is det.
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
    remove_element_liste(CS, M, M1),                % Appelle récursif avec la carte à soustraire supprimé de la main
    !.
remove_element_liste(CS, [C|M], [C|M1]) :-
    C \= CS,                                        % On s'assure que la carte à supprimer est différente de la carte en tête de liste
    remove_element_liste(CS, M, M1),                % Appelle récursif pour traiter la carte suivante de la main
    !.

%! get_max_score_combi(+CSS, -S2, -P2, -CB2, -C2, +T3) is det.
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
    piles_defausse_possible(T3, PS),                                    % On récupère les piles où il est possible de se défausser
    !,
    choisir_pile_min_points(T3, PS, P).                                 % On récupère la pile opposé à celle qui contient la carte avec le moins de points
get_max_score_combi([(P1,CB,C,S)|[]], S, P2, CB, C, _) :-
    ( ( P1 == pile_1, P2 = pile_2 ) ; ( P2 = pile_1 ) ),                % La pile de défausse est celle opposé à celle ou la carte qui nous intéresse se trouve
    !.
get_max_score_combi([(P1,CB1,C1,S1)|CSS], S2, P2, CB2, C2, _) :-
    get_max_score_combi(CSS, S3, P3, CB3, C3, _),                       % Appelle récursif pour traiter toutes les combinaisons possibles avec les cartes en sommet de pile et notre main
    ((
        S1 > S3,                                                        % Si le nombre de points de la combinaison courante est supérieur au nombre de points de la combinaison retourné
        S2 = S1,                                                        % On met à jour les informations à retourner avec les informations de la combinaison courante.
        C2 = C1,
        CB2 = CB1,
        ( ( P1 == pile_1, P2 = pile_2 ) ; ( P2 = pile_1 ) )             % La pile de défausse est celle opposé à celle ou la carte qui nous intéresse se trouve
    ) ; (                                                               % Sinon
        S2 = S3,                                                        % On met à jour les informations à retourner avec les informations de la combinaison retournée.
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
   min_points_pile(T1, V1),                                             % On récupère la valeur minimal parmi les cartes de la pile 1
   min_points_pile(T2, V2),                                             % On récupère la valeur minimal parmi les cartes de la pile 2
   ( V1 =< V2 -> P = pile_2                                             % Si la valeur max de la pile 1 est inferieur ou égale à celle de la pile 2, on défausse sur la pile 2
   ; V1 > V2 -> P = pile_1 ),                                           % Sinon, on défausse sur la pile 1
   !.

%! min_points_pile(+T: [carte], -P) is det.
%
% Retourne la pile qui n'est pas celle ou la plus petite carte se trouve.
%
% @arg T        Le sommet d'une pile sur la table
% @arg P        Les points
%
% @throws Precondition.    /
%
% @throws Postcondition.   Retourne les points de la plus petite carte de la pile.
%
min_points_pile([], 1000) .                                           % Si la pile est vide, alors on lui donne un point très important pour s'obliger à poser dessus
min_points_pile(T, P) :-
  findall((V, K), (member(K, T), carte(K, V, _)), VCS),
  argmin_list(C, VCS),                                                % on récupère la carte avec le minimum de points,
  points_carte(C, P),
  !.

%! check_pile(+T, +CS, +N, -CSS) is det.
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
   findall((N, CCS, C, P1), (member(C, T), combinaison([C|CS], CB), cartes_combinaison(CCS, CB), length(CCS, NCS), NCS is L + 1, points_cartes(CS, P1)), CSS). % On récupère les informations de chaque combinaison possible avec une carte donnée et
                                                                                                                                                               % une combinaison déjà existante ( Numéro de pile de la carte, Liste des cartes formant la combinaison
                                                                                                                                                               % La carte à piocher pour former la combinaison, Points totals de la combinaison )

%! check_piles_for_combi(+T3, +CS, -TLS) is det.
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
   check_pile_for_combi(T1, M, pile_1, CSS1),           % Récupération de toutes les combinaisons possibles avec les cartes sur la pile 1 une et notre main
   check_pile_for_combi(T2, M, pile_2, CSS2),           % Récupération de toutes les combinaisons possibles avec les cartes sur la pile 2 une et notre main
   append(CSS1, CSS2, CSS),                             % Fusion de toutes les combinaisons possibles
   !.

%! check_pile_for_combi(+T, +M, +N, -CSS) is det.
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
   findall((N, CS, C, P), (member(C, T), combinaisons([C|M], CB), CB \= [], select_combi_max(CB, C, L, CS), cartes_combinaison(CB_max, CS), points_cartes(CB_max, P)), CSS), % On récupère les informations de chaque combinaison possible avec une carte donnée et
                                                                                                                                                                             % notre main ( Numéro de pile de la carte, Liste des cartes formant la combinaison
                                                                                                                                                                             % La carte à piocher pour former la combinaison, Points totals de la combinaison )
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
   findall((P, CS), (member(CS, CB), cartes_combinaison(KS, CS), member(C,KS), length(KS, NCS), L > NCS, points_cartes(KS, P)), KPS), % Tri des combinaisons pour ne garder que celles qui contiennent la carte donnéeen paramètre
   argmax_list(CS_max, KPS),                                                                                                          % Récupération de la combinaison avec le nombre de points maximal
   !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PIOCHE
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%! recup_pioche_opti(+T3: sommets, +M: [carte], +N_P: atom, -C: { carte, 'pioche' }) is det.
%
% Retourne la carte qu'il faut piocher.
%
% @arg T3       Les sommets des trois piles sur la table
% @arg M        La main
% @arg N_P      Le nombre de cartes dans la pioche
% @arg C        La carte ramassée, éventuellement dans la pioche
%
% @throws Precondition.    /
%
% @throws Postcondition.   Retourne forcément une carte à prendre ou indique de piocher.
%
recup_pioche_opti(T, M, N_P, C) :-
   length(M, L),
   % On récupère les combinaisons faisable avec une carte de la pile et on les pondères.
   findall((P, CT), (member(CT, T), combinaison([CT|M], CB), cartes_combinaison(CS, CB), length(CS, NCS), L > NCS, points_cartes(CS, P)), CSS),
   length(CSS, CSSL),
   (( CSSL > 0, argmax_list(C, CSS) )                             % S'il y a au moins une combinaison faisable
   ; ( CSSL == 0, defaut_pioche(T, N_P, C) )),                    % S'il n'y a pas de combinaison faisable
   !.

%! defaut_pioche(+T3: sommets, +N_P, -C: { carte, 'pioche' }) is det.
%
% Retourne les actions à faire part défaut lors de la pioche s'il n'y a pas de combinaisons disponibles.
%
% @arg T3       Les sommets des trois piles sur la table
% @arg N_P      Le nombre de cartes dans la pioche
% @arg C        La carte ramassée, éventuellement dans la pioche
%
% @throws Precondition.    /
%
% @throws Postcondition.   Retourne forcément une carte de poids minimum à prendre ou indique de piocher.
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ANNONCE
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%! min_points_piles(+T3: sommets, -P) is det.
%
% Retourne les points de la plus petite carte présente sur les deux piles.
%
% @arg T3       Les sommets des trois piles sur la table
% @arg P        Les points de la plus petite carte
%
% @throws Precondition.    /
%
% @throws Postcondition.   Retourne les points de la plus petite carte.
%
min_points_piles(sommets(T1, T2, _), P) :-
   append(T1, T2, CS),                                                 % on concat les deux piles
   findall((V, K), (member(K, CS), carte(K, V, _)), VCS),
   argmin_list(C, VCS),                                                % on récupère la carte avec le minimum de points,
   points_carte(C, P),
   !.



