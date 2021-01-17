% gvim:  fileencoding=utf8

/** <module> Jeu de cartes "Moins de neuf"
 *
 * @author José Martinez, October 15-November 17, 2020
 * @see [[Polytechnic School of the University of Nantes][http://web.polytech.univ-nantes.fr/]]
 * @license proprietary
 *
 * This code cannot be reproduced, copied, transmitted, stored outside, etc., without the previous explicit authorisation of the author and/or the School.
 * It is provided only for a pedagogical usage.
 *
 * @version 1.0
 *
 * === ALWAYS DOCUMENT AND TEST YOUR CODE!
 *
 * Documentation and tests are always much longer than your actual code...
 * Never neglect the usefulness of the latter with an untyped (or dynamically typed) language, especially when changing some line of code here and there, e.g., correcting, maintaining, refactoring.
 *
 * To see the documentation of this module:
 *
 *    - start the interactive interpreter at the shell prompt:  "swipl";
 *    - run the HTTP documentation server: "doc_server(4000).";
 *    - load this module:  "consult('moins_de_neuf.pl').";
 *    - start your Web browser (with javascript enabled):  "doc_browser.".
 *    - "Et voilà!"
 *
 * To run the unit tests of this module and imported ones too:
 *
 *    - simply type in:  "run_tests.";
 *    - specific predicates can be tested individually with:  "run_tests(<predicate>)" or "run_tests([<predicate 1>, ..., <predicate n>]).".
 *
 * ===
 *
 * Le "moins de neuf" est un jeu de cartes simple, rapide à jouer mais pas totalement dépourvu d'intérêt.
 * Il mélange hasard et tactique, information et déduction.
 * Le code fourni ici permet de gérer tous les éléments _déterministes_ d'une partie.
 * 
 * __Nota.__  La quasi totalité des prédicats doit être déterministe car, dans le cas contraire, le mécanisme de retours en arrière amènerait à jouer toutes les parties !
 *
 * Les "Intelligences artificielles", ou agents intelligents, se retrouvent dans la même situation qu'un joueur humain.
 * À elles d'exploiter tout à la fois les informations pour bien jouer et l'expérience pour s'améliorer !
 *
 * Cela s'effectue en ajoutant une ou plusieurs nouvelles stratégies, dans :
 *
 *   * 'strategie/1'.
 *
 * Il faut ensuite qu'au moins un des joueurs l'utilise, dans :
 *
 *   * 'joueur/3'.
 *
 * Enfin, le plus important, et le plus difficile bien entendu, et d'étendre les trois prédicats "intelligents", à savoir :
 *
 *   * 'annonce_strategique/7' ;
 *
 *   * 'defausse_strategique/8 ;
 *
 *   * 'pioche_strategique/7'.
 *
 * __Nota.__  Il aurait été préférable d'isoler ces prédicats-là dans un module en charge des agents.
 *            Mais cela aurait nécessité, pour les tests, de faire appel de manière croisée aux prédicats de ce module.
 *            Le problème des références croisées entre modules est très délicat (bien que solvable) mais il n'est pas bien géré par SWI-Prolog.
 *
 * __Nota.__  Des points d'interactions supplémentaires peuvent être créés pour mieux guider les agents intelligents dans leurs interactions et leur connaissance de l'état d'une partie.
 *
 * === CONVENTIONS
 *
 * Le typage des données est un grand absent de Prolog (avec l'exception notable de Visual Prolog).
 * Plus exactement, il n'y a qu'un seul et unique type : le terme, structure hiérarchique multi-niveaux constituée de listes et n-uplets emboîtés récursivement jusqu'aux atomes (XML et JSON n'ont -- presque -- rien inventé !).
 *
 * Voici les pseudo-types importants du programme :
 *
 *    * carte : { 0, ..., 51 } ---  il s'agit d'un entier naturel entre 0 et 51 identifiant de manière unique chaque carte du jeu ;
 *    * valeur : { 0, ..., 12 } --- il s'agit d'un entier naturel entre 0 et 12 identifiant de manière unique chaque carte dans une enseigne ;
 *    * enseigne : { 0, ..., 3 } --- il s'agit d'un entier naturel entre 0 et 3 identifiant de manière unique chaque enseigne ;
 *    * combinaison : { [carte], paire([carte]), brelan([carte]), carre([carte]), suite([carte]) } --- il s'agit d'un ensemble de cartes, éventuellement qualifié par sa qualité : paire, brelan, carré ou suite ;
 *    * piles : piles([[carte]], [[carte]], [carte)) --- il s'agit de la structure des trois piles posées sur la table : deux visibles avec des combinaisons, et une pioche ;
 *    * sommets : sommets([carte], [carte], nat) --- il s'agit de la partie visible des trois piles posées sur la table : les cartes (restantes) de la dernière combinaison posée sur les piles visibles et le nombre de cartes restantes dans la pioche (mieux connu qu'avec une inspection visuelle !) ;
 *    * pli : { 'moins de neuf', pli({ 'aucune defausse', carte, combinaison }, { 'aucune pioche', 'pioche', carte } --- il s'agit des actions des joueurs, soit une annonce, soit une éventuelle défausse suivie d'une pioche obligatoire (sauf cas rarissime) ;
 *    * defausse : { 'aucune defausse', defausse([carte], {pile_1, pile_2}) } --- il s'agit de l'action éventuelle de défausse d'un joueur ;
 *    * pioche : { carte, 'pioche' } --- il s'agit de l'action obligatoire de pioche sur la table d'un joueur ;
 *    * brain : term --- il s'agit vraiment d'une information quelconque, un terme Prolog, gérée par chaque "Intelligence artificielle".
 *
 * __Nota.__  Les atomes 'aucune defausse' et 'aucune pioche' ne sont pas utiles dans cette variante du jeu.
 *
 * Le dictionnaire des variables est, de manière incomplète, le suivant, les ambiguïtés étant levées par le contexte :
 *
 *    * A : le joueur ayant lancé l'annonce "moins de neuf" ;
 *    * B : l'état du cerveau ("brain") d'un joueur ;
 *    * C : une carte ;
 *    * CS : un ensemble de cartes ;
 *    * CSS : un ensemble d'ensemble de cartes ;
 *    * D : le donneur ou la défausse ;
 *    * E : l'enseigne d'une carte (communément dénommée "couleur");
 *    * J : un jeu de carte ou un joueur ;
 *    * JS : un ensemble de joueurs ;
 *    * K : une combinaison de cartes ;
 *    * KS : des combinaisons de cartes ;
 *    * M : les cartes dans une main ;
 *    * MS : les mains des différents joueurs ;
 *    * N : un nombre (joueurs, cartes, manches, etc.) ;
 *    * P : pile, pioche, pli ou points ;
 *    * PS : les plis précédents, les paires ou les perdants ;
 *    * P3 : les trois piles présentes sur la table ;
 *    * S : une stratégie de jeu, ou le score d'un joueur ;
 *    * T3 : les trois sommets de piles présentes sur la table (le nombre de cartes pour la pioche) ;
 *    * V : une valeur de carte ;
 *    * VS : une liste de valeurs de cartes.
 *
 * De manière générale, quelques règle de nommage reviennent :
 *
 *    * Chaque "S" en suffixe indique une liste ou ensemble de ce qui précède (ex. : C pour une carte, puis CS pour une liste de cartes et enfin CSS pour une liste de listes de cartes).
 *    * Le suffixe "_post" est souvent employé pour distinguer l'état d'une "même" variable après une "action" ou "changement d'état".
 *    * Le préfixe "N_" est souvent employé pour représenter un nombre de ce qui suit (ex. : N_M pour le nombre de manches, N_P pour le nombre de cartes dans la pioche, etc.)
 *    * Les suffixes "_min", "_max", "_sorted", "_shuffle", etc., permettent de préciser clairement le rôle d'une variable.
 *    * L'infixe "_moins_" ou "_plus_" permet également de préciser à quoi correspond une variable dont un nom arbitraire rendrait la compréhension trop malaisée.
 *    * Enfin, les n-uplets sont le plus souvent décrits par le concaténation des lettres correspondant aux composantes, éventuellement avec un S final pour les listes de n-uplets (ex. : JMBS traduit une liste (S final) de triplets constitués d'un joueur J, de sa main M et de son cerveau B).
 *
 * __Nota.__  Ce code source est relativement long bien que son cœur ne représente qu'un peu plus de 10 % seulement (environ 500 lignes), après avoir enlevé les tests, les commentaires et les parties non essentielles (comme les mélanges de cartes mais surtout toutes les fausses "Intelligences artificielles") !
 *            Il est fortement conseillé de le parcourir avec le navigateur pour bien isoler visuellement les commentaires, les tests et le code effectif.
 *
 * @bug   La partie interactive, avec un joueur humain, n'a pas été totalement testée.
 * @bug   Toutes les préconditions réunies consomment le plus de temps d'exécution (environ 18 % pour une partie avec les agents pseudo intelligents fournis).
 *        Toutefois, il n'est bien sûr pas raisonnable de les commenter en phase de développement !
 * @bug   Le tour final de jeu n'a pas été développé.
 *        Cela revient à avoir modifié les règles pour arrêter une manche dès l'annonce.
 *        Pourtant, cela constitue une partie intéressante dans la réflexion, notamment lorsque l'on va perdre !
 *        Sans stratégie particulière sur cette phase finale du jeu par les "Intelligences artificielles", on peut en rester là.
 * @bug   En cas de rechargement du code source, les post-conditions du prédicat 'gagnants_manche/3' (d'où 'perdants_manche/3',  'scores_manche/3', etc.) ne sont plus satisfaites (?!).
 *        Un même exemple va fonctionner au premier chargement, ne plus fonctionner au second et fonctionner de nouveau si l'on relance SWI-Prolog !
 *        Le débogueur graphique permet de vérifier que l'on ne parcourt pas le même code...
 *        Cela est vrai pour la version 7.6.4 et peut avoir été corrigé dans les versions ultérieures, la dernière en date étant la 8.2.3-1.
 * @bug   Tous les problèmes n'ont sans doute pas été répertoriés...
 *
 */

:- module(moins_de_neuf,
   [ % Cartes

     valeur/3
   , enseigne/2
   , carte/3
   , jeu/1
   , pretty_carte/2
   , pretty_cartes/2

     % Mélanges

   , full_random_shuffle/2
   , manual_random_shuffle/2

     % Combinaisons

   , paire/2
   , paires/2
   , brelan/2
   , brelans/2
   , carre/2
   , carres/2
   , suite/2
   , suites/2
   , combinaison/2
   , cartes_combinaison/2
   , combinaisons/2
   , points_carte/2
   , points_cartes/2

     % Sommets

   , sommets_vides/1
   , deposer_cartes_sommets/3
   , retirer_carte_sommets/3

     % Piles

   , sommets_piles/2
   , piocher/3
   , remplacer_sommet_multipile/3
   , remplacer_sommets_piles/3
   , piles_defausse_possible/2

     % Joueurs

   , joueur/3
   , joueurs/1
   , nombre_de_joueurs/1

     % Distribution

   , nombre_de_cartes_initiales_par_joueur/1
   , distribution/3

     % Pli et stratégies

   , strategie/1
   , defausse/7
   , defausse_strategique/8
   , pioche/7
   , pioche_imposee/2
   , pioche_strategique/7
   , annonce/7
   , annonce_strategique/7
   , pli/9
   , jouer_pli/9

     % Manches

   , nombre_manches/1

   ]).

:- use_module(utilities, [ all_different/1
                         , all_equal/1
                         , sublist/2
                         , take/3
                         , take_at_most/3
                         , drop/3
                         , multiply/3
                         , interleave/3
                         , argmax_list/2
                         , argmin_list/2
                         , rotate/2
                         ]).
:- use_module(quickcheck, [ is_random_subset_of/2
                          , is_non_empty_random_subset/2
                          , is_random_subset_of_at_most/3
                          , is_non_empty_random_subset_of_at_most/3
                          , is_random_subset_of_between/3
                          , is_random_subset_of_exactly/3
                          ]).

:- use_module(skynet, [ min_points_piles/2,
                        choisir_pile_min_points/3,
                        recup_pioche_opti/4,
                        check_piles_for_combi/3,
                        get_max_score_combi/6,
                        remove_elements_liste/3,
                        defausse_pioche_opti/2
]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% CARTES
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Les prédicats suivants permettent de définir un jeu de 52 cartes.
% Chaque carte est identifiée par un numéro interne, un simple entier compris entre 0 et 51
% Chacune est aussi décrite par sa valeur, depuis as jusqu'à roi et son enseigne : cœur, carreau, pique ou trèfle.
% Pour ce qui est de la valeur, il y a la valeur faciale mais aussi la valeur numérique associée.
%
% Deux prédicats permettent de traduire textuellement une carte et un ensemble de cartes, 'pretty_carte/2' et 'pretty_cartes/2' respectivement.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%! valeur(+C: carte, -V: atom, -P: nat) is semidet.
%! valeur(-C: carte, +V: atom, -P: nat) is semidet.
%! valeur(-C: carte, -V: atom, +P: nat) is nondet.
%
% Ensemble des valeurs autorisées des cartes avec leur représentation graphique associée.
%
% @arg C   Numéro de valeur d'une carte, compris entre 0 et 12
% @arg V   Valeur visuelle associée
% @arg P   Nombre de points associés
%
% @throws Postcondition.   Le numéro d'une valeur est une clé du prédicat.
% @throws Postcondition.   La valeur visuelle valeur est aussi une clé du prédicat.
% @throws Postcondition.   La valeur visuelle associée ne peut pas être une chaîne vide.
% @throws Postcondition.   Les points associés sont positifs.
%
% @bug  La stratégie de 'pioche_strategique/7' gloutonne doit être modifiée si les valeurs des cartes le sont.
%
valeur( 0,  '1',  1).
valeur( 1,  '2',  2).
valeur( 2,  '3',  3).
valeur( 3,  '4',  4).
valeur( 4,  '5',  5).
valeur( 5,  '6',  6).
valeur( 6,  '7',  7).
valeur( 7,  '8',  8).
valeur( 8,  '9',  9).
valeur( 9, '10', 10).
valeur(10,  'V', 10).
valeur(11,  'D', 10).
valeur(12,  'R', 10).

:- begin_tests(valeur).

test(numero_cle) :-
   assertion( forall( ( valeur(C, _, _)
                      , findall(V, valeur(C, V, _), VS)
                      )
                    , length(VS, 1)
                    ) ).

test(valeur_cle) :-
   assertion( forall( ( valeur(_, V, _)
                      , findall(C, valeur(C, V, _), NS)
                      )
                    , length(NS, 1)
                    ) ).

test(valeur_non_vide) :-
   assertion( forall( valeur(_, V, _)
                    , V \= ""
                    ) ).

test(points_positifs) :-
   assertion( forall( valeur(_, _, P)
                    , P >= 0
                    ) ).

:- end_tests(valeur).

%! enseigne(+E: carte, -V: atom) is semidet.
%! enseigne(-E: carte, +V: atom) is semidet.
%! enseigne(?E: carte, ?V: atom) is nondet.
%
% Ensemble des enseignes d'un jeu standard de cartes.
%
% @arg E   Une enseigne
% @arg V   L'enseigne visuelle associée
%
% @throws Postcondition.   Le numéro d'une enseigne est une clé du prédicat.
% @throws Postcondition.   L'enseigne visuelle est aussi une clé du prédicat.
% @throws Postcondition.   L'enseigne visuelle associée ne peut pas être une chaîne vide.
%
enseigne(0, '♥').
enseigne(1, '♦').
enseigne(2, '♣').
enseigne(3, '♠').

:- begin_tests(enseigne).

test(enseigne_cle) :-
   assertion( forall( ( enseigne(E, _)
                      , findall(V, enseigne(E, V), VS)
                      )
                    , length(VS, 1)
                    ) ).

test(visuel_cle) :-
   assertion( forall( ( enseigne(_, V)
                      , findall(E, enseigne(E, V), ES)
                      )
                    , length(ES, 1)
                    ) ).

test(enseigne_non_vide) :-
   assertion( forall( enseigne(_, V)
                    , V \= ''
                    ) ).

:- end_tests(enseigne).

%! carte(+C: carte, +V: valeur, +E: enseigne) is semidet.
%! carte(+C: carte, +V: valeur, -E: enseigne) is semidet.
%! carte(+C: carte, -V: valeur, +E: enseigne) is semidet.
%! carte(+C: carte, -V: valeur, -E: enseigne) is semidet.
%! carte(-C: carte, +V: valeur, +E: enseigne) is semidet.
%! carte(-C: carte, +V: valeur, -E: enseigne) is nondet.
%! carte(-C: carte, -V: valeur, +E: enseigne) is nondet.
%! carte(-C: carte, -V: valeur, -E: enseigne) is nondet.
%
% Carte d'un jeu standard de 52 cartes.
%
% @arg C   Numéro de carte
% @arg V   Valeur associée
% @arg E   Enseigne visuelle associée
%
% @throws Postcondition.   Le numéro d'une carte est une clé du prédicat.
% @throws Postcondition.   La valeur et la enseigne forment aussi une clé composite du prédicat.
% @throws Postcondition.   La valeur est une valeur valide.
% @throws Postcondition.   L'enseigne est une enseigne valide.
%
% @bug  L'implémentation est très lourde pour être plus efficace.
%       Tous les cas d'instanciation des variables sont passés en revue pour leur associer la meilleure façon de les résoudre.
%       Sinon la deuxième définition suffirait, sans les 'nonvar', mais avec une complexité en 4.12.52.O(1), soit jusqu'à 52 fois plus que la plus coûteuse ci-dessous.
%       Notons qu'utiliser la bibliothèque clp(fd) aurait été possible pour obtenir un code court mais pas nécessairement aussi efficace.

%       En pratique, après analyse avec le profileur ('profile/1'), il reste le prédicat le plus coûteux de tout le code de ce module, environ un quart du temps d'une partie (!), lui-même dominé par les 'is/2' à plus de 50 % !
%       Mettre la première définition en tête, car elle sera la plus utilisée, et ajouter des coupures après chaque liste de contrôles des instanciations, permet de descendre à 17 % du temps d'exécution.
%
carte(C, V, E) :-     nonvar(C),    var(V),    var(E),   %      O(1)
   !,
   between(0, 51, C),
   V is C div 4, valeur(V, _, _),
   E is C mod 4, enseigne(E, _),
   C =:= V * 4 + E.
carte(C, V, E) :-     nonvar(C), nonvar(V), nonvar(E),   %      O(1)
   !,
   between(0, 51, C),
   valeur(V, _, _),
   enseigne(E, _),
   C =:= V * 4 + E.
carte(C, V, E) :-     nonvar(C), nonvar(V),    var(E),   %      O(1)
   !,
   between(0, 51, C),
   valeur(V, _, _),
   E is C mod 4, enseigne(E, _),
   C =:= V * 4 + E.
carte(C, V, E) :-     nonvar(C),    var(V), nonvar(E),   %      O(1)
   !,
   between(0, 51, C),
   V is C div 4, valeur(V, _, _),
   enseigne(E, _),
   C =:= V * 4 + E.
carte(C, V, E) :-        var(C), nonvar(V), nonvar(E),   %      O(1)
   !,
   valeur(V, _, _),
   enseigne(E, _),
   C is V * 4 + E,
   between(0, 51, C).
carte(C, V, E) :-        var(C), nonvar(V),    var(E),   %    4.O(1)
   !,
   valeur(V, _, _),
   enseigne(E, _),
   C is V * 4 + E,
   between(0, 51, C).
carte(C, V, E) :-        var(C),    var(V), nonvar(E),   %   12.O(1)
   !,
   valeur(V, _, _),
   enseigne(E, _),
   C is V * 4 + E,
   between(0, 51, C).
carte(C, V, E) :-        var(C),    var(V),    var(E),   % 12.4.O(1)
   !,
   valeur(V, _, _),
   enseigne(E, _),
   C is V * 4 + E,
   between(0, 51, C).

:- begin_tests(carte).

test(numero_cle) :-
   assertion( forall( ( carte(C, _, _)
                      , findall((V, E), carte(C, V, E), VCS)
                      )
                    , length(VCS, 1)
                    ) ).

test(valeur_enseigne_cle) :-
   assertion( forall( ( carte(_, V, E)
                      , findall(C, carte(C, V, E), NS)
                      )
                    , length(NS, 1)
                    ) ).

test(valeur_valide) :-
   assertion( forall( carte(_, V, _)
                    , valeur(V, _, _)
                    ) ).

test(enseigne_valide) :-
   assertion( forall( carte(_, _, E)
                    , enseigne(E, _)
                    ) ).

:- end_tests(carte).

%! jeu(?J: [carte]) is det.
%
% Prédicat constant renvoyant ou vérifiant l'ensemble des cartes d'un jeu de 52 cartes, bien ordonné.
%
% @arg J   La liste de toutes les cartes du jeu
%
% @throws Postcondition.   Le jeu contient bien 52 cartes.
% @throws Postcondition.   Il s'agit d'un _ensemble_ de cartes.
% @throws Postcondition.   Il s'agit bien de cartes (leur numéro plus exactement).
%
jeu(J) :-
   findall( C
          , carte(C, _, _)
          , J
          ).

:- begin_tests(jeu).

test(det) :-
   assertion( aggregate_all(count, jeu(_), 1) ).

test(jeu_52_cartes) :-
   jeu(J),
   assertion( length(J, 52) ).

test(ensemble) :-
   jeu(J),
   assertion( is_set(J) ).

test(cartes) :-
   jeu(J),
   assertion( forall( member(C, J)
                    , carte(C, _, _)
                    ) ).

:- end_tests(jeu).

%! pretty_carte(+C: carte, -T: atom) is det.
%! pretty_carte(-C: carte, +T: atom) is semidet.
%! pretty_carte(-C: carte, -T: atom) is nondet.
%
% Associe une représentation textuelle à une carte.
%
% @arg C   Une carte (un numéro de carte plus exactement)
% @arg T   La représentation textuelle de la carte
%
% @throws Postcondition.   L'atome n'est pas vide.
%
pretty_carte(C, T) :-
   carte(C, V, E),
   valeur(V, T_V, _),
   enseigne(E, T_E),
   format_to_chars('~w~w', [T_V, T_E], T_C),
   atom_codes(T, T_C).

:- begin_tests(pretty_carte).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , random_member(C, J)
                      )
                    , aggregate_all(count, pretty_carte(C, _), 1)
                    ) ).

test(non_vide) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , random_member(C, J)
                      )
                    , ( pretty_carte(C, T)
                      , T \= ''
                      )
                    ) ).

:- end_tests(pretty_carte).

%! pretty_cartes(+K: union(combinaison, [carte]), -T: atom) is det.
%
% Associe une représentation visuelle à un ensemble de cartes, éventuellement vide.
%
% @arg K   Une combinaison de cartes (ou directement un ensemble de cartes)
% @arg T   La représentation textuelle de la main
%
% @throws Postcondition.   L'atome est vide si, et seulement si, la liste est vide.
%
pretty_cartes(paire(K), T) :-
   assertion( nonvar(K) ),
   maplist(pretty_carte, K, TS),
   atomic_list_concat([paire | TS], ' ', T).
pretty_cartes(brelan(K), T) :-
   assertion( nonvar(K) ),
   maplist(pretty_carte, K, TS),
   atomic_list_concat([brelan | TS], ' ', T).
pretty_cartes(carre(K), T) :-
   assertion( nonvar(K) ),
   maplist(pretty_carte, K, TS),
   atomic_list_concat([carre | TS], ' ', T).
pretty_cartes(suite(K), T) :-
   assertion( nonvar(K) ),
   maplist(pretty_carte, K, TS),
   atomic_list_concat([suite | TS], ' ', T).
pretty_cartes(K, T) :-
   assertion( nonvar(K) ),
   maplist(pretty_carte, K, TS),
   atomic_list_concat(TS, ' ', T).

:- begin_tests(pretty_cartes).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(K, J)
                      )
                    , aggregate_all(count, pretty_cartes(K, _), 1)
                    ) ).

test(non_vide_ssi_non_vide) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(K, J)
                      )
                    , ( pretty_cartes(K, T)
                      , ( T  = '' -> K  = []
                        ; T \= '' -> K \= []
                        ) % (T = '') = (K = []) compile mais ne fonctionne pas...
                      )
                    ) ).

:- end_tests(pretty_cartes).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% MÉLANGES
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Un jeu de cartes doit être mélangé.
% Mais une permutation totalement aléatoire ne correspond pas exactement à la façon dont un jeu réel est battu.
% Plusieurs prédicats permettent d'approcher un peu ce comportement.
% L'intérêt est que l'on ne peut plus faire l'hypothèse d'équiprobabilité sur la façon dont les cartes vont être distribuées et sortir progressivement de la pioche !
%
% On pourra commencer par une permutation vraiment aléatoire au tout début d'une partie.
% Ensuite, on procédera à des mélanges dits manuels au début de chaque manche, en reprenant tout d'abord les cartes dans les mains et les piles plutôt que depuis un jeu neuf et en les mélangeant par un nombre aléatoire de coupures.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%! full_random_shuffle(+L: [a], -M: [a]) is det.
%! full_random_shuffle(-L: [a], +M: [a]) is det.
%
% Permutation aléatoire d'une liste.
%
% Appliquée aux cartes, cela donne un comportement très différent d'un mélange manuel.
% Il sera appliqué au tout début d'une partie, sur un paquet neuf, mais ensuite ce sont des mélanges manuels qui seront appliqués.
%
% @arg L   Une liste d'éléments
% @arg M   Une liste des mêmes éléments permutés
%
% @throws Postcondition.   Les deux listes contiennent exactement les mêmes éléments, avec leur même nombre d'occurrences.
%
full_random_shuffle(L, M) :-
   random_permutation(L, M).

:- begin_tests(random_shuffle).

test(det_1) :-
   jeu(J),
   assertion( forall( between(1, 100, _)
                    , full_random_shuffle(J, _)
                    ) ).

test(det_2) :-
   jeu(J),
   assertion( forall( between(1, 100, _)
                    , full_random_shuffle(_, J)
                    ) ).

test(sans_perte_ni_ajout) :-
   jeu(J),
   sort(J, J_sorted),
   assertion( forall( ( between(1, 100, _)
                      , full_random_shuffle(J, K)
                      )
                    , sort(K, J_sorted)
                    ) ).

:- end_tests(random_shuffle).

%! single_shuffle_at(?L: [a], ?P: nat, ?M: [a]) is nondet.
%! single_shuffle_at(+L: [a], ?P: nat, ?M: [a]) is multi.
%! single_shuffle_at(+L: [a], +P: nat, ?M: [a]) is det.
%! single_shuffle_at(+L: [a], +P: nat, +M: [a]) is semidet.
%
% Permutation entre le préfixe de longueur indiqué et le suffixe de la liste.
% Cela correspond à une "coupe" d'un paquet de cartes.
%
% @arg L   Une liste d'éléments
% @arg P   Une longueur / position pour le préfixe de la liste
% @arg M   La liste obtenue en intervertissant le préfixe de taille P et son suffixe complémentaire
%
% @throws Precondition.   La longueur demandée ne doit pas être plus grande que la longueur de la liste.
%
% @throws Postcondition.   L'opération est inversible en prenant comme indice la longueur de la liste moins la valeur indiquée.
%
% @bug  La version sans argument instancié amène à une recherche infinie, ce qui est normal, mais elle est très largement incomplète (se limitant à la position zéro pour des listes de plus en plus longues...).
%
single_shuffle_at(L, P, _) :-
   nonvar(L),
   nonvar(P),
   length(L, N_L),
   N_L < P,
   format("Precondition 'single_shuffle_at(~w, ~w, _)' violated:  index out of range\n", [L, P]),
   assertion( false ).
%
single_shuffle_at(L, P, M) :-
   append(L1, L2, L),
   length(L1, P),
   append(L2, L1, M).

:- begin_tests(single_shuffle_at).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(M, J)
                      , length(M, N_M)
                      , random_between(0, N_M, P)
                      )
                    , aggregate_all(count, single_shuffle_at(M, P, _), 1)
                    ) ).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(M, J)
                      , length(M, N_M)
                      , plus(N_M, 1, N_M_plus_1)
                      )
                    , aggregate_all(count, single_shuffle_at(M, _, _), N_M_plus_1)
                    ) ).

test(inverse_a_gauche) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(M, J)
                      , length(M, N_M)
                      , random_between(0, N_M, P)
                      , single_shuffle_at(M, P, ES)
                      , Q is N_M - P
                      )
                    , single_shuffle_at(ES, Q, M)
                    ) ).

:- end_tests(single_shuffle_at).

%! random_single_shuffle(+L: [a], -M: [a]) is det.
%
% Permutation entre le préfixe de longueur indiqué et le suffixe de la liste.
% Cela correspond à une "coupe" d'un paquet de cartes à une position aléatoire.
% En pratique, cela s'effectue plutôt près de la moitié du paquet.
% Cette version est donc encore un peu différente de la réalité.
% Mais l'équiprobabilité permet d'éviter le cas limite où l'on ne ferait qu'un échange des deux moitiés !
% En pratique, cela s'obtient par le fait d'effectuer plusieurs petites coupes sur la principale.
%
% @arg XS   Une liste d'éléments
% @arg YS   Une liste obtenue en intervertissant un préfixe de taille aléatoire et le suffixe complémentaire
%
% @throws Postcondition.   L'opération est inversible en retrouvant la position de l'échange.
%
random_single_shuffle(L, M) :-
   assertion( nonvar(L) ),
   length(L, N_L),
   random_between(0, N_L, P),
   single_shuffle_at(L, P, M).

:- begin_tests(random_single_shuffle).

test(det) :-
   jeu(J),
   assertion( forall( between(1, 100, _)
                    , aggregate_all(count, random_single_shuffle(J, _), 1)
                    ) ).

test(inverse_a_gauche) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(M, J)
                      , random_single_shuffle(M, ES)
                      )
                    , ( append(FS, _, ES)
                      , length(FS, P)
                      , single_shuffle_at(ES, P, M)
                      )
                    ) ).

:- end_tests(random_single_shuffle).

%! random_multiple_shuffle(+L: [a], +N: nat, ?M: [a]) is det.
%
% Permutation entre le préfixe de longueur indiqué et le suffixe de la liste.
% Cela correspond à battre un paquet de carte, ce qui fournit une permutation des cartes mais est loin d'aboutir à une permutation complètement aléatoire, sauf à battre et rebattre un très grand nombre de fois.
%
% @arg L   Un jeu de cartes
% @arg N   Un nombre de fois où le paquet est "coupé", à des positions aléatoires
% @arg M   Le jeu de cartes mélangé
%
% @throws Postcondition.   Le jeu mélangé est une permutation du jeu de cartes.
%
random_multiple_shuffle(L, 0, L).
random_multiple_shuffle(L, N, M) :-
   assertion( (nonvar(L), nonvar(N)) ),
   N > 0,
   plus(N_minus_1, 1, N),
   random_multiple_shuffle(L, N_minus_1, P),
   full_random_shuffle(P, M).

:- begin_tests(random_multiple_shuffle).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 20, N)
                      )
                    , aggregate_all(count, random_multiple_shuffle(J, N, _), 1)
                    ) ).

test(permutation) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 20, N)
                      , random_multiple_shuffle(J, N, K)
                      )
                    , permutation(J, K)
                    ) ).

:- end_tests(random_multiple_shuffle).

%! manual_random_shuffle_min_max(?N_min: nat, ?N_max: nat) is det.
%
% Indication empirique sur le nombre de fois où un joueur bat un paquet de cartes pour le mélanger.
%
% @arg N_min   Nombre minimal de fois où un jeu est coupé pour être mélangé
% @arg N_max   Nombre maximal de fois où un jeu est coupé pour être mélangé
%
% @throws Postcondition.   Le nombre minimal de fois est un entier naturel.
% @throws Postcondition.   Le nombre maximal de fois est supérieur ou égal au nombre minimal.
%
manual_random_shuffle_min_max(8, 16).

:- begin_tests(manual_random_shuffle_min_max).

test(det) :-
   assertion( aggregate_all(count, manual_random_shuffle_min_max(_, _), 1) ).

test(min_non_negatif) :-
   manual_random_shuffle_min_max(N_min, _),
   assertion( N_min >= 0 ).

test(max_non_inferieur) :-
   manual_random_shuffle_min_max(N_min, N_max),
   assertion( N_min =< N_max ).

:- end_tests(manual_random_shuffle_min_max).

%! manual_random_shuffle(+L: [a], ?M: [a]) is det.
%
% Mélange aléatoire se voulant plus proche de la réalité lorsque l'on bat un paquet de cartes.
% Certaines cartes de la manche précédente ont tendance à rester positionner les unes à côté des autres.
% Celles de tête seront plus ou moins finement mélangées lors de la distribution en fonction du nombre de joueurs.
% Celles qui restent dans la pioche le seront dans des conditions similaires.
%
% @arg L   Un jeu de cartes
% @arg M   Le jeu de cartes mélangé
%
% @throws Postcondition.   Le jeu mélangé est une permutation du jeu de cartes.
%
manual_random_shuffle(L, M) :-
   assertion( nonvar(L) ),
   manual_random_shuffle_min_max(N_min, N_max),
   random_between(N_min, N_max, N),
   random_multiple_shuffle(L, N, M).

:- begin_tests(manual_random_shuffle).

test(det) :-
   jeu(J),
   assertion( forall( between(1, 100, _)
                    , aggregate_all(count, manual_random_shuffle(J, _), 1)
                    ) ).

test(permutation) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , manual_random_shuffle(J, K)
                      )
                    , permutation(J, K)
                    ) ).

:- end_tests(manual_random_shuffle).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% COMBINAISONS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Le "moins de neuf" est un jeu de combinaisons.
% Il s'agit des classiques :
%
%    * paires,
%
%    * brelans,
%
%    * carrés et
%
%    * suites (de longueur au moins égale à trois).
%
% Par ailleurs, il s'agit d'un jeu de comptage.
% À partir des points associés à chaque carte, on construit le nombre de points d'un ensemble de cartes comme leur simple somme.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%! paire(+M: [carte], -CS: [carte]) is nondet.
%
% Paire présente dans une main.
%
% @arg M    Les cartes dans une main
% @arg CS   Une liste de deux cartes constituant une paire de la main
%
% @throws Postcondition.   Il y a deux cartes exactement dans une paire.
% @throws Postcondition.   Les deux cartes sont bien présentes dans la main.
% @throws Postcondition.   Elles ont même valeur.
% @throws Postcondition.   Elles sont différentes.
%
paire(M, [C1, C2]) :-
   assertion( nonvar(M) ),
   append(_, [C1 | CS], M),
   append(_, [C2 | _], CS),
   C1 \= C2,
   carte(C1, V, _),
   carte(C2, V, _).

:- begin_tests(paire).

test(deux) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , paire(M, P)
                      )
                    , length(P, 2)
                    ) ).

test(cartes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , paire(M, [C1, C2])
                      )
                    , ( carte(C1, V, _)
                      , carte(C2, V, _)
                      )
                    ) ).

test(presentes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , paire(M, [C1, C2])
                      )
                    , ( member(C1, M)
                      , member(C2, M)
                      )
                    ) ).

test(differentes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , paire(M, P)
                      )
                    , all_different(P)
                    ) ).

:- end_tests(paire).

%! paires(+M: [carte], -CSS: [[carte]]) is det.
%
% Ensemble de toutes les paires présente dans une main.
%
% @arg M     Les cartes dans une main
% @arg CSS   La liste de tous les couples de cartes constituant une paire de la main
%
% @throws Postcondition.   Il y a deux cartes exactement dans chaque paire.
% @throws Postcondition.   Les deux cartes sont bien présentes dans la main.
% @throws Postcondition.   Les cartes d'une même paire ont même valeur.
% @throws Postcondition.   Elles sont différentes.
%
% @bug  Peut générer jusqu'à 12 * 6 = 72 paires, mais pour le jeu complet seulement.
%
paires(M, CSS) :-
   assertion( nonvar(M) ),
   findall( CS
          , paire(M, CS)
          , CSS
          ).

:- begin_tests(paires).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      )
                    , aggregate_all(count, paires(M, _), 1)
                    ) ).

test(deux_cartes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , paires(M, PS)
                      )
                    , forall( member(P, PS)
                            , length(P, 2)
                            )
                    ) ).

test(cartes_presentes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , paires(M, PS)
                      )
                    , forall( member([C1, C2], PS)
                            , ( member(C1, M)
                              , member(C2, M)
                              )
                            )
                    ) ).

test(cartes_presentes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , paires(M, PS)
                      )
                    , forall( member([C1, C2], PS)
                            , ( carte(C1, V, _)
                              , carte(C2, V, _)
                              )
                            )
                    ) ).

:- end_tests(paires).

%! brelan(+M: [carte], -CS: [carte]) is nondet.
%
% Brelan présent dans une main.
%
% @arg M    Les cartes dans une main
% @arg CS   Une liste de trois cartes constituant un brelan de la main
%
% @throws Postcondition.   Il y a trois cartes exactement dans un brelan.
% @throws Postcondition.   Les trois cartes sont bien présentes dans la main.
% @throws Postcondition.   Elles ont même valeur.
% @throws Postcondition.   Elles sont différentes.
%
brelan(M, [C1, C2, C3]) :-
   assertion( nonvar(M) ),
   append(_, [C1 | CS], M),
   append(_, [C2 | DS], CS),
   carte(C1, V, _),
   carte(C2, V, _),
   C1 \= C2,
   append(_, [C3 | _S], DS),
   C1 \= C3,
   C2 \= C3,
   carte(C3, V, _).

:- begin_tests(brelan).

test(trois) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , brelan(M, B)
                      )
                    , length(B, 3)
                    ) ).

test(cartes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , brelan(M, [C1, C2, C3])
                      )
                    , ( carte(C1, V, _)
                      , carte(C2, V, _)
                      , carte(C3, V, _)
                      )
                    ) ).

test(presentes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , brelan(M, [C1, C2, C3])
                      )
                    , ( member(C1, M)
                      , member(C2, M)
                      , member(C3, M)
                      )
                    ) ).

test(differentes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , brelan(M, B)
                      )
                    , all_different(B)
                    ) ).

:- end_tests(brelan).

%! brelans(+M: [carte], -CSS: [[carte]]) is det.
%
% Ensemble de tous les brelans présents dans une main.
%
% @arg M     Les cartes dans une main
% @arg CSS   La liste de tous les triplets de cartes constituant un brelan de la main
%
% @throws Postcondition.   Il y a trois cartes exactement dans chaque brelan.
% @throws Postcondition.   Les trois cartes sont bien présentes dans la main.
% @throws Postcondition.   Les cartes d'une même brelan ont même valeur.
% @throws Postcondition.   Elles sont différentes.
%
% @bug  Peut générer jusqu'à 12 * 4 = 48 brelans, mais pour le jeu complet seulement.
%
brelans(M, CSS) :-
   assertion( nonvar(M) ),
   findall( CS
          , brelan(M, CS)
          , CSS
          ).

:- begin_tests(brelans).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      )
                    , aggregate_all(count, brelans(M, _), 1)
                    ) ).

test(trois) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , brelans(M, BS)
                      )
                    , forall( member(B, BS)
                            , length(B, 3)
                            )
                    ) ).

test(cartes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , brelans(M, BS)
                      )
                    , forall( member([C1, C2, C3], BS)
                            , ( carte(C1, V, _)
                              , carte(C2, V, _)
                              , carte(C3, V, _)
                              )
                            )
                    ) ).

test(presentes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , brelans(M, BS)
                      )
                    , forall( member([C1, C2, C3], BS)
                            , ( member(C1, M)
                              , member(C2, M)
                              , member(C3, M)
                              )
                            )
                    ) ).

test(differentes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , brelans(M, BS)
                      )
                    , forall( member(B, BS)
                            , all_different(B)
                            )
                    ) ).

:- end_tests(brelans).

%! carre(+M: [carte], -CS: [carte]) is nondet.
%
% Carré présent dans une main.
%
% @arg M    Les cartes dans une main
% @arg CS   Une liste de quatre cartes constituant un carré de la main
%
% @throws Postcondition.   Il y a quatre cartes exactement dans un carré.
% @throws Postcondition.   Les quatre cartes sont bien présentes dans la main.
% @throws Postcondition.   Elles ont même valeur.
% @throws Postcondition.   Elles sont différentes.
%
carre(M, [C1, C2, C3, C4]) :-
   assertion( nonvar(M) ),
   append(_, [C1 | CS], M),
   append(_, [C2 | DS], CS),
   carte(C1, V, _),
   carte(C2, V, _),
   append(_, [C3 | ES], DS),
   carte(C3, V, _),
   append(_, [C4 | _], ES),
   carte(C4, V, _),
   all_different([C1, C2, C3, C4]).

:- begin_tests(carre).

test(quatre) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , carre(M, C)
                      )
                    , length(C, 4)
                    ) ).

test(cartes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , carre(M, [C1, C2, C3, C4])
                      )
                    , ( carte(C1, V, _)
                      , carte(C2, V, _)
                      , carte(C3, V, _)
                      , carte(C4, V, _)
                      )
                    ) ).

test(presentes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , carre(M, [C1, C2, C3, C4])
                      )
                    , ( member(C1, M)
                      , member(C2, M)
                      , member(C3, M)
                      , member(C4, M)
                      )
                    ) ).

test(differentes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , carre(M, C)
                      )
                    , all_different(C)
                    ) ).

:- end_tests(carre).

%! carres(+M: [carte], -CSS: [[carte]]) is det.
%
% Ensemble de tous les carrés présents dans une main.
%
% @arg M     Les cartes dans une main
% @arg CSS   La liste de tous les quadruplets de cartes constituant un carré de la main
%
% @throws Postcondition.   Il y a quatre cartes exactement dans chaque carré.
% @throws Postcondition.   Les quatre cartes sont bien présentes dans la main.
% @throws Postcondition.   Les cartes d'une même carré ont même valeur.
% @throws Postcondition.   Elles sont différentes.
%
% @bug  Peut générer jusqu'à 12 carrés, mais pour le jeu complet seulement.
%
carres(M, CSS) :-
   assertion( nonvar(M) ),
   findall( CS
          , carre(M, CS)
          , CSS
          ).

:- begin_tests(carres).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      )
                    , aggregate_all(count, carres(M, _), 1)
                    ) ).

test(quatre) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , carres(M, KS)
                      )
                    , forall( member(K, KS)
                            , length(K, 4)
                            )
                    ) ).

test(cartes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , carres(M, KS)
                      )
                    , forall( member([C1, C2, C3, C4], KS)
                            , ( carte(C1, V, _)
                              , carte(C2, V, _)
                              , carte(C3, V, _)
                              , carte(C4, V, _)
                              )
                            )
                    ) ).

test(presentes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , carres(M, KS)
                      )
                    , forall( member([C1, C2, C3, C4], KS)
                            , ( member(C1, M)
                              , member(C2, M)
                              , member(C3, M)
                              , member(C4, M)
                              )
                            )
                    ) ).

test(differentes) :-
   jeu(J),
   assertion( forall( ( between(1, 30, _)
                      , is_random_subset_of(M, J)
                      , carres(M, KS)
                      )
                    , forall( member(K, KS)
                            , all_different(K)
                            )
                    ) ).

:- end_tests(carres).

%! suite(+M: [carte], -CS: [carte]) is nondet.
%
% Suite présente dans une main.
%
% @arg M           Les cartes dans une main
% @arg CS_sorted   Une liste de cartes formant une suite dans une main, triée par valeurs croissantes
%
% @throws Postcondition.   La suite contient au moins trois cartes.
% @throws Postcondition.   La suite est une sous-liste des cartes de la main.
% @throws Postcondition.   Elles sont différentes.
% @throws Postcondition.   Toutes les enseignes d'une suite sont les mêmes.
% @throws Postcondition.   Toutes les valeurs d'une suite sont consécutives.
%
% @bug  On peut générer jusqu'à 220 suites, mais pour le jeu complet seulement.
% @bug  L'algorithme est très clair mais malheureusement exponentiel...
%       Heureusement, cela ne sera pas une difficulté durant une partie réelle où le nombre de cartes en main est limité.
%
suite(M, CS) :-
   assertion( nonvar(M) ),
   sublist(CS, M),
   longueur_au_moins(CS, 3),
   enseignes_identiques(CS),
   valeurs_consecutives(CS).

%! longueur_au_moins(+L, +N_min) is semidet.
%
longueur_au_moins(L, N_min) :-
   assertion( (nonvar(L), nonvar(N_min)) ),
   length(L, N_L),
   N_L >= N_min.

%! enseignes_identiques(+CS) is semidet.
%
enseignes_identiques([]).
enseignes_identiques([C1 | CS]) :-
   carte(C1, _, E),
   forall(member(C, CS), carte(C, _, E)).

%! valeurs_consecutives(+CS) is semidet.
%
valeurs_consecutives([]).
valeurs_consecutives(CS) :-
   assertion( nonvar(CS) ),
   findall(V, (member(C, CS), carte(C, V, _)), VS), % maplist([C, V]>>carte(C, V, _), CS, VS), % est trop lent...
   min_list(VS, V_min),
   max_list(VS, V_max),
   V_diff is V_max - V_min + 1,
   length(VS, V_diff).

:- begin_tests(suite).

test(au_moins_trois) :-
   jeu(J),
   assertion( forall( ( between(1, 3, _)
                      , is_random_subset_of_at_most(M, J, 11)
                      , suite(M, CS)
                      )
                    , ( length(CS, N_CS)
                      , N_CS >= 3
                      )
                    ) ).

test(cartes) :-
   jeu(J),
   assertion( forall( ( between(1, 3, _)
                      , is_random_subset_of_at_most(M, J, 11)
                      , suite(M, CS)
                      )
                    , forall( member(C, CS)
                            , carte(C, _, _)
                            )
                    ) ).

test(presentes) :-
   jeu(J),
   assertion( forall( ( between(1, 3, _)
                      , is_random_subset_of_at_most(M, J, 11)
                      , suite(M, CS)
                      )
                    , forall( member(C, CS)
                            , member(C, M)
                            )
                    ) ).

test(differentes) :-
   jeu(J),
   assertion( forall( ( between(1, 3, _)
                      , is_random_subset_of_at_most(M, J, 11)
                      , suite(M, CS)
                      )
                    , all_different(CS)
                    ) ).

test(meme_enseigne) :-
   jeu(J),
   assertion( forall( ( between(1, 3, _)
                      , is_random_subset_of_at_most(M, J, 11)
                      , suite(M, CS)
                      )
                    , ( maplist([C, E]>>carte(C, _, E), CS, ES)
                      , all_equal(ES)
                      )
                    ) ).

:- end_tests(suite).

%! suites(+M: [carte], -CSS: [[carte]]) is det.
%
% Ensemble de toutes les suites présentes dans une main.
%
% @arg M     Les cartes dans une main
% @arg CSS   La liste de toutes les suites de cartes
%
% @throws Postcondition.   Chaque suite contient au moins trois cartes.
% @throws Postcondition.   Chaque suite est une sous-liste des cartes de la main.
% @throws Postcondition.   Toutes les cartes d'une suite sont différentes.
% @throws Postcondition.   Toutes les cartes d'une suite sont de même enseigne.
% @throws Postcondition.   Les valeurs de chaque suite sont consécutives.
%
suites(M, CSS) :-
   assertion( nonvar(M) ),
   findall( CS
          , suite(M, CS)
          , CSS
          ).

:- begin_tests(suites).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 3, _)
                      , is_random_subset_of_at_most(M, J, 11)
                      )
                    , aggregate_all(count, suites(M, _), 1)
                    ) ).

:- end_tests(suites).

%! combinaison(+M: [carte], -K: { paire([carte]), brelan([carte]), carre([carte]), suite([carte])}) is nondet.
%
% Combinaison dans une main, qu'il s'agisse d'une paire, d'un brelan, d'un carré ou d'une suite.
%
% @arg M   Les cartes dans une main
% @arg K   Une des combinaisons présente dans la main
%
% @throws Postcondition.   Le "foncteur" ne peut être que l'atome 'paire', 'brelan', 'carre' ou 'suite'.
%
combinaison(M, paire(P)) :-
   assertion( nonvar(M) ),
   paire(M, P).
combinaison(M, brelan(B)) :-
   brelan(M, B).
combinaison(M, carre(K)) :-
   carre(M, K).
combinaison(M, suite(S)) :-
   suite(M, S).

:- begin_tests(combinaison).

test(foncteur) :-
   jeu(J),
   assertion( forall( ( between(1, 3, _)
                      , is_random_subset_of_at_most(M, J, 11)
                      , combinaison(M, K)
                      )
                    , ( K = paire(_)
                      ; K = brelan(_)
                      ; K = carre(_)
                      ; K = suite(_)
                      )
                    ) ).

:- end_tests(combinaison).

%! cartes_combinaison(-CS: [carte], +K: union(combinaison, [carte])) is det.
%! cartes_combinaison(+CS: [carte], +K: union(combinaison, [carte])) is semidet.
%
% Extraction des cartes d'une combinaison ou vérification que des cartes constitue une combinaison.
%
% @arg CS   Les cartes d'une combinaison
% @arg K    Une combinaison de cartes
%
% @throws Postcondition.   Le "foncteur" ne peut être que l'atome 'paire', 'brelan', 'carre' ou 'suite'.
%
cartes_combinaison(CS, paire(CS)) :-
   paire(CS, CS).
cartes_combinaison(CS, brelan(CS)) :-
   brelan(CS, CS).
cartes_combinaison(CS, carre(CS)) :-
   carre(CS, CS).
cartes_combinaison(CS, suite(CS)) :-
   suite(CS, CS).
cartes_combinaison([C | CS], [C | CS]).

%! combinaisons(+M: [carte], -KS: [combinaison]) is det.
%
% Ensemble des combinaisons dans une main.
%
% @arg M    Les cartes dans une main
% @arg KS   Toutes les combinaisons présentes dans la main
%
% @bug   Attention, le temps d'exécution de ce prédicat est exponentiel en fonction du nombre de cartes !
%
combinaisons(M, KS) :-
   assertion( nonvar(M) ),
   paires(M,  PS),
   brelans(M, BS),
   carres(M,  CS),
   suites(M,  SS),
   /*
   findall(A, ( member(paire(A),  PS)
              ; member(brelan(A), BS)
              ; member(carre(A),  CS)
              ; member(suite(A),  SS)
              ), KS).
   */
   maplist([A, B]>>(B = paire(A) ), PS, PS2),
   maplist([A, B]>>(B = brelan(A)), BS, BS2),
   maplist([A, B]>>(B = carre(A) ), CS, CS2),
   maplist([A, B]>>(B = suite(A) ), SS, SS2),
   append([PS2, BS2, CS2, SS2], KS).

:- begin_tests(combinaisons).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of_at_most(M, J, 11)
                      )
                    , aggregate_all(count, combinaisons(M, _), 1)
                    ) ).

:- end_tests(combinaisons).

%! points_carte(+C: carte, -P: nat) is det.
%
% Points d'une cartes.
%
% @arg C   Une carte
% @arg P   Les points associés à la carte
%
% @throws Postcondition.   Les points d'une carte est un entier naturel.
%
points_carte(C, P) :-
   assertion( nonvar(C) ),
   carte(C, V, _),
   valeur(V, _, P).

:- begin_tests(points_carte).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , random_member(C, J)
                      )
                    , aggregate_all(count, points_carte(C, _), 1)
                    ) ).

test(points_non_negatif) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , random_member(C, J)
                      , points_carte(C, P)
                      )
                    , P >= 0
                    ) ).

:- end_tests(points_carte).

%! points_cartes(+M: [carte], -T: nat) is det.
%
% Points cumulés contenus dans un ensemble de cartes.
%
% @arg M   Les cartes dans une main, un pli, une combinaison
% @arg T   Le total des points des cartes
%
%
points_cartes(M, T) :-
   assertion( nonvar(M) ),
   aggregate_all( sum(P)
                , ( member(C, M)
                  , points_carte(C, P)
                  )
                , T
                ).

:- begin_tests(points_cartes).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(M, J)
                      )
                    , aggregate_all(count, points_cartes(M, _), 1)
                    ) ).

test(total_points_non_negatif) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(M, J)
                      , points_cartes(M, T)
                      )
                    , T >= 0
                    ) ).

:- end_tests(points_cartes).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% SOMMETS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Sur la table, les joueurs ne peuvent voir que le sommet des trois piles de cartes.
% Les deux piles visibles sont en fait des "multi-piles" dans le sens où il s'agit de piles de combinaisons, donc de piles de sous-ensembles de cartes.
% Toutes les cartes d'une combinaison préalablement posées se trouvent simultanément visibles et ramassables, une à la fois par joueur.
% En revanche, la pioche, qui contient les cartes retournées, est une pile de simples cartes.
% Du point de vue du joueur, soit elle est vide, soit il reste encore des cartes dedans, mais il ne peut pas savoir quelle carte il va bien pouvoir en tirer.
%
% Pendant un pli, un joueur peut, s'il le souhaite, déposer une combinaison sur la table, dans l'un des deux tas visibles.
% À défaut, ou en alternative, il peut se défausser d'une simple carte.
% Il devra ensuite piocher une, et une seule, carte, soit parmi les cartes visibles autres que sa défausse, soit dans la pioche.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%! sommets_vides(-T3: sommets) is det.
%! sommets_vides(+T3: sommets) is semidet.
%
% @arg T3   Les trois sommets des piles de cartes
%
sommets_vides(sommets([], [], 0)).

%! random_sommets(+CS: [carte], +P_max: (nat, nat, nat), -T3: sommets) is det.
%
% Génération de pseudo sommets afin de tester les post-conditions de certains prédicats.
%
% @arg CS      Les cartes pouvant être distribuées dans les sommets visibles
% @arg P_max   Le nombre maximal de cartes dans chaque sommet
% @arg T3      Les trois sommets des piles de cartes
%
% @throws Postcondition.   Les cardinaux maximaux ne doivent pas être négatifs.
%
% @throws Postcondition.   Les cartes des sommets visibles ont été prises parmi celles fournies.
%
random_sommets(CS, (P1_max, P2_max, P_max), sommets(T1, T2, N_P)) :-
   assertion( (nonvar(CS), nonvar(P_max)) ),
   assertion( (P1_max >= 0, P2_max >= 0, P_max >= 0) ),
   is_random_subset_of_at_most(T1, CS, P1_max),
   subtract(CS, T1, CS_moins_T1),
   is_random_subset_of_at_most(T2, CS_moins_T1, P2_max),
   random_between(0, P_max, N_P).

:- begin_tests(random_sommets).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(CS, J)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      )
                    , aggregate_all(count, random_sommets(CS, (P1_max, P2_max, P_max), _), 1)
                    ) ).

test(cartes_existantes_pile1) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(CS, J)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , random_sommets(CS, (P1_max, P2_max, P_max), sommets(T1, _, _))
                      )
                    , forall( member(C, T1)
                            , member(C, CS)
                            )
                    ) ).

test(cartes_existantes_pile2) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(CS, J)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , random_sommets(CS, (P1_max, P2_max, P_max), sommets(_, T2, _))
                      )
                    , forall( member(C, T2)
                            , member(C, CS)
                            )
                    ) ).

test(nombre_cartes_pile1) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(CS, J)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , random_sommets(CS, (P1_max, P2_max, P_max), sommets(T1, _, _))
                      )
                    , ( length(T1, N1)
                      , N1 =< P1_max
                      )
                    ) ).

test(nombre_cartes_pile2) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(CS, J)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , random_sommets(CS, (P1_max, P2_max, P_max), sommets(_, T2, _))
                      )
                    , ( length(T2, N2)
                      , N2 =< P2_max
                      )
                    ) ).

test(nombre_cartes_pioche) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(CS, J)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , random_sommets(CS, (P1_max, P2_max, P_max), (_, _, N_P))
                      )
                    , N_P =< P_max
                    ) ).

:- end_tests(random_sommets).

%! random_sommets(+CS: [carte], -T3: sommets) is det.
%
% @arg CS   Les cartes pouvant être distribuées dans les sommets visibles
% @arg T3   Les trois sommets des piles de cartes
%
random_sommets(CS, T3) :-
   assertion( nonvar(CS) ),
   random_sommets(CS, (3, 3, 3), T3).

%! random_sommets_non_vides(+CS: [carte], +P_max: (nat, nat, nat), -T3: sommets) is det.
%
% @arg CS      Les cartes pouvant être distribuées dans les sommets visibles
% @arg P_max   Le nombre maximal de cartes dans chaque sommet
% @arg T3      Les trois sommets des piles de cartes
%
% @throws Precondition.   Il doit y avoir au moins une carte à placer.
% @throws Precondition.   Au moins l'un des cardinaux doit être strictement positif.
%
% @throws Postcondition.   Les cartes des sommets visibles ont été prises parmi celles fournies.
%
random_sommets_non_vides([], _, _) :-
   format("Precondition 'random_sommets_non_vides([], _, _)' violated:  empty hand\n", []),
   assertion( false ).
random_sommets_non_vides(_, (0, 0, 0), _) :-
   format("Precondition 'random_sommets_non_vides(_, (0, 0, 0, _)' violated:  all cardinals are nulls\n", []),
   assertion( false ).
%
random_sommets_non_vides(CS, (P1_max, P2_max, P_max), sommets(T1, T2, N_P)) :-
   assertion( (nonvar(CS), nonvar(P_max)) ),
   is_random_subset_of_at_most(T1, CS, P1_max),
   subtract(CS, T1, CS_moins_T1),
   is_random_subset_of_at_most(T2, CS_moins_T1, P2_max),
   random_between(0, P_max, N_P),
   \+ sommets_vides(sommets(T1, T2, N_P)),
   !.
random_sommets_non_vides(CS, PS, T3) :-
   random_sommets_non_vides(CS, PS, T3).

:- begin_tests(random_sommets_non_vides).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_non_empty_random_subset(CS, J)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , (P1_max >= 1 ; P2_max >= 1 ; P_max >= 1)
                      )
                    , aggregate_all(count, random_sommets_non_vides(CS, (P1_max, P2_max, P_max), _), 1)
                    ) ).

test(non_vides) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_non_empty_random_subset(CS, J)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , (P1_max >= 1 ; P2_max >= 1 ; P_max >= 1)
                      , random_sommets_non_vides(CS, (P1_max, P2_max, P_max), T3)
                      )
                    , \+ sommets_vides(T3)
                    ) ).

test(cartes_existantes_pile1) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_non_empty_random_subset(CS, J)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , (P1_max >= 1 ; P2_max >= 1 ; P_max >= 1)
                      , random_sommets_non_vides(CS, (P1_max, P2_max, P_max), sommets(T1, _, _))
                      )
                    , forall( member(C, T1)
                            , member(C, CS)
                            )
                    ) ).

test(cartes_existantes_pile2) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_non_empty_random_subset(CS, J)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , (P1_max >= 1 ; P2_max >= 1 ; P_max >= 1)
                      , random_sommets_non_vides(CS, (P1_max, P2_max, P_max), sommets(_, T2, _))
                      )
                    , forall( member(C, T2)
                            , member(C, CS)
                            )
                    ) ).

test(nombre_cartes_pile1) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_non_empty_random_subset(CS, J)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , (P1_max >= 1 ; P2_max >= 1 ; P_max >= 1)
                      , random_sommets_non_vides(CS, (P1_max, P2_max, P_max), sommets(T1, _, _))
                      )
                    , ( length(T1, N1)
                      , N1 =< P1_max
                      )
                    ) ).

test(nombre_cartes_pile2) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_non_empty_random_subset(CS, J)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , (P1_max >= 1 ; P2_max >= 1 ; P_max >= 1)
                      , random_sommets_non_vides(CS, (P1_max, P2_max, P_max), sommets(_, T2, _))
                      )
                    , ( length(T2, N2)
                      , N2 =< P2_max
                      )
                    ) ).

test(nombre_cartes_pioche) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_non_empty_random_subset(CS, J)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , (P1_max >= 1 ; P2_max >= 1 ; P_max >= 1)
                      , random_sommets_non_vides(CS, (P1_max, P2_max, P_max), sommets(_, _, N_P))
                      )
                    , N_P =< P_max
                    ) ).

:- end_tests(random_sommets_non_vides).

%! random_sommets_non_vides(+CS: [carte], -T3: sommets) is det.
%
% @arg CS   Les cartes pouvant être distribuées dans les sommets visibles
% @arg T3   Les trois sommets des piles de cartes
%
random_sommets_non_vides(CS, T3) :-
   assertion( nonvar(CS) ),
   random_sommets_non_vides(CS, (3, 3, 3), T3).

%! deposer_cartes_sommets(+T3: sommets, +D: defausse, +T3_post: sommets) is det.
%
% @arg T3        Les trois sommets des piles de cartes
% @arg D         La défausse à poser
% @arg T3_post   Les trois sommets des piles de cartes
%
% @throws Precondition.   Si des cartes sont effectivement déposées, il doit y en avoir !
% @throws Precondition.   Si des cartes sont effectivement déposées, la pile doit être 1 ou 2.
%
% @throws Postcondition.   Le sommets résultant ne peut être vide que si l'on ne pose rien sur un sommet déjà vide.
% @throws Postcondition.   Au moins une des deux piles visibles n'est pas modifiée.
%
deposer_cartes_sommets(_, defausse(K, _), _) :-
   cartes_combinaison([], K),
   format("Precondition 'deposer_cartes_sommets(_, defausse(~w, _), _)' violated:  empty combination\n", [K]),
   assertion( false ).
deposer_cartes_sommets(_, defausse(_, P), _) :-
   \+ member(P, [pile_1, pile_2]),
   format("Precondition 'deposer_cartes_sommets(_, defausse(_, ~w), _)' violated:  invalid stack\n", [P]),
   assertion( false ).
%
deposer_cartes_sommets(T3, 'aucune defausse', T3) :-
   assertion( nonvar(T3) ).
deposer_cartes_sommets(sommets(_, T2, N_P), defausse(K, pile_1), sommets(CS, T2, N_P)) :-
   assertion( nonvar(K) ),
   cartes_combinaison(CS, K).
deposer_cartes_sommets(sommets(T1, _, N_P), defausse(K, pile_2), sommets(T1, CS, N_P)) :-
   cartes_combinaison(CS, K).

:- begin_tests(deposer_cartes_sommets).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of_at_most(CS, J, 5)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , random_sommets(J, (P1_max, P2_max, P_max), T3)
                      , ( CS  = [] -> D = 'aucune defausse'
                        ; CS \= [] -> random_member(P, [pile_1, pile_2]),
                                      D = defausse(CS, P)
                        )
                      )
                    , aggregate_all(count, deposer_cartes_sommets(T3, D, _), 1)
                    ) ).

test(vide_si_rien_sur_vide) :-
   sommets_vides(T3_vide),
   assertion( deposer_cartes_sommets(T3_vide, 'aucune defausse', T3_vide) ).

test(non_vide_si_rien_sur_non_vide) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , random_sommets(J, (P1_max, P2_max, P_max), T3)
                      , \+ sommets_vides(T3)
                      , deposer_cartes_sommets(T3, 'aucune defausse', T3_post)
                      )
                    , \+ sommets_vides(T3_post)
                    ) ).

test(non_vide_si_cartes_sur_non_vide) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , random_sommets(J, (P1_max, P2_max, P_max), T3)
                      , \+ sommets_vides(T3)
                      , is_non_empty_random_subset_of_at_most(CS, J, 5)
                      , random_member(P, [pile_1, pile_2])
                      , deposer_cartes_sommets(T3, defausse(CS, P), T3_post)
                      )
                    , \+ sommets_vides(T3_post)
                    ) ).

test(non_vide_si_cartes_sur_vide) :-
   jeu(J),
   sommets_vides(T3_vide),
   assertion( forall( ( between(1, 100, _)
                      , is_non_empty_random_subset_of_at_most(CS, J, 5)
                      , random_member(P, [pile_1, pile_2])
                      , deposer_cartes_sommets(T3_vide, defausse(CS, P), T3_post)
                      )
                    , \+ sommets_vides(T3_post)
                    ) ).

test(au_moins_une_pile_inchangee) :-
   jeu(J),
   sommets_vides(T3_vide),
   assertion( forall( ( between(1, 100, _)
                      , is_non_empty_random_subset_of_at_most(CS, J, 5)
                      , random_member(P, [pile_1, pile_2])
                      , deposer_cartes_sommets(T3_vide, defausse(CS, P), sommets(T1, T2, _))
                      )
                    , ( T1 \= []
                      ; T2 \= []
                      )
                    ) ).

:- end_tests(deposer_cartes_sommets).

%! retirer_carte_sommets(+T3: sommets, +C: { carte, 'pioche' }, +T3_post: sommets) is det.
%
% @arg T3        Les trois sommets des piles de cartes
% @arg C         La carte devant être retirée, ou l'atome 'pioche'
% @arg T3_post   Les trois sommets des piles de cartes
%
% @throws Precondition.   La carte, si elle n'est pas 'pioche', doit être présente dans l'une ou l'autre des deux piles visibles.
% @throws Precondition.   Si la carte est pioche, la pile de la pioche doit contenir au moins une carte.
%
retirer_carte_sommets(sommets(T1, T2, _), C, _) :-
   assertion( (nonvar(T1), nonvar(T2), nonvar(C)) ),
   C \= pioche,
   \+ member(C, T1),
   \+ member(C, T2),
   format("Precondition 'retirer_carte_sommets((sommets(~w, ~w, _), ~w, _)' violated:  non-visible card\n", [T1, T2, C]),
   assertion( false ).
retirer_carte_sommets(sommets(_, _, 0), pioche, _) :-
   format("Precondition 'retirer_carte_sommets/3' violated:  empty invisible stack\n", []),
   assertion( false ).
%
retirer_carte_sommets(sommets(T1, T2, N_P), C, sommets(T1_moins_C, T2, N_P)) :-
   assertion( nonvar(N_P) ),
   member(C, T1),
   delete(T1, C, T1_moins_C).
retirer_carte_sommets(sommets(T1, T2, N_P), C, sommets(T1, T2_moins_C, N_P)) :-
   member(C, T2),
   delete(T2, C, T2_moins_C).
retirer_carte_sommets(sommets(T1, T2, N_P), pioche, sommets(T1, T2, N_P_moins_1)) :-
   plus(N_P_moins_1, 1, N_P).

:- begin_tests(retirer_carte_sommets).

test(det_pioche) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(CS, J)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , random_sommets(CS, (P1_max, P2_max, P_max), T3)
                      , T3 = sommets(_, _, N_P)
                      , N_P > 0
                      )
                    , retirer_carte_sommets(T3, pioche, _)
                    ) ).

test(det_carte) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(CS, J)
                      , random_between(0, 52, P1_max)
                      , random_between(0, 52, P2_max)
                      , random_between(0, 52, P_max)
                      , random_sommets(CS, (P1_max, P2_max, P_max), T3)
                      , T3 = sommets(T1, T2, _)
                      , ( T1 \= []
                        ; T2 \= []
                        )
                      , append(T1, T2, T)
                      , member(C, T)
                      )
                    , retirer_carte_sommets(T3, C, _)
                    ) ).

:- end_tests(retirer_carte_sommets).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PILES
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% L'arbitre du jeu, c'est-à-dire l'ordinateur qui remplace la réalité, doit connaître le contenu exact et détaillé des trois piles (ainsi que des mains des joueurs et autres informations associées).
% Les décisions qui seront prises par les joueurs sur l'état visible seront transformées en actions sur l'état global, aussi bien visible qu'invisible, par l'arbitre.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%! sommets_piles(-T3: sommets, +P3: piles) is det.
%
% La première étape lorsqu'un joueur joue est, éventuellement, de se défausser de certaines de ces cartes, celles pouvant former une combinaison, puis d'en piocher obligatoirement une dans l'un des sommets (non vide).
%
% @arg T3   Les trois sommets des trois piles sur la table
% @arg P3   Les trois piles sur la table
%
sommets_piles(sommets(T1, T2, N_P), piles([T1 | _], [T2 | _], PS)) :-
   assertion( (nonvar(T1), nonvar(T2), nonvar(PS)) ),
   length(PS, N_P).
sommets_piles(sommets([], [], N_P), piles([], [], PS)) :-
   length(PS, N_P).
sommets_piles(sommets(T1, [], N_P), piles([T1 | _], [], PS)) :-
   length(PS, N_P).
sommets_piles(sommets([], T2, N_P), piles([], [T2 | _], PS)) :-
   length(PS, N_P).

%! piocher(+P3: sommets, -T_P: carte, -P3_post: piles) is det.
%! piocher(-P3: sommets, +T_P: carte, +P3_post: piles) is det.
%
% La première étape lorsqu'un joueur joue est, éventuellement, de se défausser de certaines de ces cartes, celles pouvant former une combinaison, puis d'en piocher obligatoirement une dans l'un des sommets (non vide).
%
% @arg P3        Les trois piles sur la table
% @arg T_P       La carte prise au sommet de la pioche
% @arg P3_post   Les trois piles sur la table après la pioche
%
% @throws Precondition.   La pioche n'est pas vide.
%
% @throws Postcondition.   La carte piochée est prise au sommet de la troisième pile.
% @throws Postcondition.   Les deux premières piles restent inchangées.
%
piocher(piles(_, _, []), _, _) :-
   format("Precondition 'piocher(piles(_, _, []), _, _)' violated:  empty invisible stack\n", []),
   assertion( false ).
%
piocher(piles(P1, P2, [T_P | PS]), T_P, piles(P1, P2, PS)).

:- begin_tests(piocher).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , distribution(J, _, P3)
                      , P3 = piles(_, _, [_ | _])
                      )
                    , aggregate_all(count, piocher(P3, _, _), 1)
                    ) ).

test(carte_piochee) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , distribution(J, _, P3)
                      , P3 = piles(_, _, [T_P | P3_post])
                      )
                    , piocher(P3, T_P, piles(_, _, P3_post))
                    ) ).

test(piles_visibles_intactes) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , distribution(J, _, P3)
                      , P3 = piles(P1, P2, [_ | _])
                      )
                    , piocher(P3, _, piles(P1, P2, _))
                    ) ).

:- end_tests(piocher).

%! remplacer_sommet_multipile(+CS: [carte], +PSS: [[carte]], -PSS_post: [[carte]]) is det.
%
% Il s'agit d'un prédicat technique.
% Seul les sommet des piles sont fournis aux "Intelligences artificielles", pour leur éviter de tricher (involontairement bien entendu !).
% Il s'agit ici de remettre en ordre les multipiles aux cartes visibles sur table en fonction des nouvelles, ou mêmes, cartes à leur sommet après un pli.
%
% @arg CS         Les nouvelles cartes du sommet de la multipile
% @arg PSS        L'état de la multipile avant le remplacement
% @arg PSS_post   L'état de la multipile après le remplacement
%
% @throws Precondition.   Une multipile peut être vide, mais aucune des (ex) combinaisons de cartes qu'elle contient ne peut l'être, en particulier son sommet.
% @throws Precondition.   Le nouveau sommet et l'ancien doivent être soit égaux, soit disjoints, soit le nouveau sommet est un sous-ensemble strict de l'ancien, de cardinal diminué de un exactement.
%
remplacer_sommet_multipile(_, [[] | _], _) :-
   format("Precondition 'remplacer_sommet_multipile(_, [[] | _], _)' violated:  no empty combination in a stack\n", []),
   assertion( false ).
remplacer_sommet_multipile(CS1, [CS2 | _], _) :-
   assertion( (nonvar(CS1), nonvar(CS2)) ),
   \+ ( CS1 = CS2
      ; intersection(CS1, CS2, [])
      ; ( subset(CS1, CS2)
        , length(CS1, N)
        , plus(N, 1, N_plus_1)
        , length(CS2, N_plus_1)
        )
      ),
   format("Precondition 'remplacer_sommet_multipile(~w, [~w | _], _)' violated:  incompatible old and new top of stack\n", [CS1, CS2]),
   assertion( false ).
%
remplacer_sommet_multipile(CS,     [CS  | PSS],         [CS | PSS]).    % Si le "nouveau" sommet de la pile est inchangé, on conserve la pile telle quelle (le sommet n'est pas vide).
remplacer_sommet_multipile(CS_new, [CS  | PSS], [CS_new     | PSS]) :-  % Si le nouveau sommet de la pile
   subset(CS_new, CS),                                                  % est un sous-ensemble
   CS_new \= [],                                                        % non-vide
   length(CS_new, N_CS_new),
   length(CS, N_CS),
   plus(N_CS_new, 1, N_CS).                                             % contenant une carte de moins, il s'agit d'une carte retirée, d'où on remplace le sommet de la pile. 
remplacer_sommet_multipile([],     [[_] | PSS],               PSS).     % Lorsque l'on a pris la dernière carte au sommet d'une multipile non vide, on dépile pour faire apparaître des cartes d'une combinaison précédente.
remplacer_sommet_multipile(CS_new, [CS  | PSS], [CS_new, CS | PSS]) :-  % Si le nouveau sommet de la pile
   intersection(CS_new, CS, []),                                        % est totalement différent de l'ancien,
   CS_new \= [].                                                        % et non vide, il s'agit de cartes posées au dessus.
remplacer_sommet_multipile(CS_new, [],          [CS_new]         ) :-   % Comme cas particulier, de nouvelles cartes peuvent être posées au sommet d'une multipile vide.
   CS_new \= [].
remplacer_sommet_multipile([],     [],          []               ).     % Comme cas particulier, une multipile initialement vide reste vide si aucune combinaison n'a été posée dessus.

:- begin_tests(remplacer_sommet_multipile).

test(det_nouvelles_cartes) :-
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of_between(CS1, [a, b, c, d, e], interval(2, 5))
                      , is_non_empty_random_subset_of_at_most(CS2, [f, g, h, i, j, k, l, m], 4)
                      )
                    , aggregate_all(count, remplacer_sommet_multipile(CS1, [CS2], _), 1)
                    ) ).

test(det_anciennes_cartes) :-
   assertion( forall( ( between(1, 100, _)
                      , is_non_empty_random_subset_of_at_most(CS2, [a, b, c, d, e, f, g, h], 8)
                      , length(CS2, N)
                      , plus(N_moins_1, 1, N)
                      , is_random_subset_of_exactly(CS1, CS2, N_moins_1)
                      )
                    , aggregate_all(count, remplacer_sommet_multipile(CS1, [CS2], _), 1)
                    ) ).

:- end_tests(remplacer_sommet_multipile).

%! remplacer_sommets_piles(+T3: sommets, +P3: piles, -P3_post: piles) is det.
%
% @arg T3        Les nouveaux sommets des piles
% @arg P3        Les trois piles sur la table
% @arg P3_post   Les trois piles sur la table après mise à jour de leurs sommets
%
% @throws Precondition.   Au plus deux sommets de pile sont modifiés.
%
remplacer_sommets_piles(sommets(CS1_new, CS2_new, N_P_new), P3, _) :-
   assertion( (nonvar(CS1_new), nonvar(CS2_new), nonvar(N_P_new), nonvar(P3)) ),
   sommets_piles(sommets(CS1, CS2, N_P), P3),
   CS1_new \= CS1,
   CS2_new \= CS2,
   N_P_new \= N_P,
   format("Precondition 'remplacer_sommets_piles(sommets(~w, ~w, ~w), ~w, _)' violated:  more than two updated stacks, sommets(~w, ~w, ~w)\n", [CS1_new, CS2_new, N_P_new, P3, CS1, CS2, N_P]),
   assertion( false ).
%
remplacer_sommets_piles(sommets(CS1, CS2, _), P3, P3) :-                                   % Comme cas particulier, seule la pioche a été modifiée, aucune des deux multipiles visibles.
   sommets_piles(sommets(CS1, CS2, _), P3),
   !.
remplacer_sommets_piles(sommets(CS1_new, CS2_new, _), P3, piles(P1_post, P2_post, PS)) :-  % Dans le cas général, on met à jour une ou les deux multipiles visibles.
   P3 = piles(P1, P2, PS),
   remplacer_sommet_multipile(CS1_new, P1, P1_post),
   remplacer_sommet_multipile(CS2_new, P2, P2_post).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% JOUEURS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Nous sommes ici au cœur du problème.
% Il s'agit, au-delà d'une utilisation interactive avec un joueur humain, de fournir une ou plusieurs "Intelligences artificielles".
% Elles sont dénommées "stratégies".
% Les stratégies fournies par défaut sont extrêmement basiques, pour ne pas dire rudimentaires.
% Elles sont au nombre de trois :
%
%    * La stratégie primitive fait toujours la première chose qui se présente.
%      Elle annonce "moins de neuf" immédiatement, si elle le peut.
%      Elle pose la première combinaison trouvée, ou la première carte en main.
%      Elle prend la première carte, soit visible dans l'un ou l'autre tas, soit dans la pioche.
%
%    * La stratégie aléatoire évite cette prévisibilité en faisant tous ses choix au hasard.
%      Elle n'annonce "moins de neuf" que trois fois sur quatre.
%      Elle pose n'importe quelle combinaison présente dans sa main mais elle peut aussi décider de ne pas le faire.
%      Elle ramasse une carte au hasard dans l'une ou l'autre des piles.
%
%    * La stratégie gloutonne procède à des choix locaux guidés par le gain immédiat.
%      Elle annonce "moins de neuf" aléatoirement mais en tenant compte de la valeur des cartes dans sa main, soit à 100 % s'il ne lui reste qu'un as (cela aurait pu être 99 % pour tenir compte du risque d'échec minime d'après les règles du jeu) mais seulement à 2 % si elle a un huit.
%      Elle pose une des combinaisons qui lui permet de se débarrasser d'un maximum de points.
%      Inversement, elle ramasse la carte de coût minimal et n'envisage de piocher que si elle y est contrainte ou qu'elle a des chances supérieures à la moyenne d'y piocher une carte moins coûteuse.
%      Cette troisième stratégie reste donc relativement prévisible aussi.
%
% Notons que les trois stratégies fournies constituent des agents à réflexes, et sans mémoire, c'est-à-dire les systèmes pseudo intelligents les plus simples, ceux qui ne font que réagir face à un stimulus extérieur.
% Employés en grand nombre dans un environnement commun, ils peuvent parfois faire apparaître une certaine forme d'intelligence collective.
% Ce ne devrait pas être le cas ici !
% (Peut-être en ne faisant jouer que des gloutons, c'est-à-dire des "traders", entre eux...)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%! joueur(?N: atom, ?S: atom, ?B: term) is nondet.
%
% Chaque joueur est identifié par un nom.
% On lui associe une stratégie de jeu.
% Surtout, il dispose d'un cerveau, dans son état initial.
% Il pourra rester vide...
% Mais il s'agit de sa mémoire à long terme, censée lui permettre d'apprendre.
% Elle pourra évoluer au fur et à mesure des parties !
% Au strict minimum, elle permet de faire passer de l'information entre les différentes phases du jeu, représentées par les prédicats suivants :
%
%    * 'annonce_strategique/7', pour déterminer si l'on annonce "moins de neuf" lorsque l'on en a la possibilité ;
%
%    * 'defausse_strategique/8', pour déterminer quelle combinaison poser sur quelle pile si cela est possible et qu'on le souhaite ;
%
%    * 'pioche_strategique/7', pour déterminer quelle carte prendre aussi bien parmi les dernières cartes visibles que sur la pioche.
%
% __Nota.__  Entre plusieurs parties, il faudra afficher le terme et le recopier dans ce fichier pour ne pas repartir à zéro.
%
% @arg N   Nom du joueur
% @arg S   Stratégie du joueur
% @arg B   État initial du cerveau ("brain") du joueur
%
% @throws Postcondition.   Le nom du joueur est une clé de la relation.
%
joueur("gloutonne", gloutonne, cerveau_glouton(0, 0, 0)).
joueur("primitive", primitive, cerveau_vide).
joueur("gloutonne2", gloutonne, cerveau_glouton(0, 0, 0)).
/*
joueur(bank2, gloutonne, cerveau_vide).
joueur(bank3, gloutonne, cerveau_vide).
joueur(bank4, gloutonne, cerveau_vide).
joueur(gork2, primitive, cerveau_vide).
joueur(gork3, primitive, cerveau_vide).
joueur('José IIIe', humaine, cerveau_humain).
joueur('José IIe', humaine, cerveau_humain).
joueur('José Ier', humaine, cerveau_humain).
*/

cerveau_defaut(primitive, cerveau_vide).
cerveau_defaut(aleatoire, cerveau_vide).
cerveau_defaut(gloutonne, cerveau_glouton(0, 0, 0)). % L'approche gloutonne compte le nombre d'actions stratégiques effectuées. Cela ne sert à rien sauf à vérifier que les cerveaux évoluent correctement.
cerveau_defaut(humaine, cerveau_humain).

:- begin_tests(joueur).

test(nom_cle) :-
   assertion( forall( ( joueur(N, _, _)
                      , findall(N, joueur(N, _, _), NS)
                      )
                    , length(NS, 1)
                    ) ).

:- end_tests(joueur).

%! nombre_de_joueurs(?N_J: nat) is det.
%
% Nombre de joueurs
%
% @arg N_J   Le nombre de joueurs connus
%
% @throws Postcondition.   Le nombre de joueurs est au moins de trois.
%
nombre_de_joueurs(N_J) :-
   aggregate_all(count, joueur(_, _, _), N_J).

:- begin_tests(nombre_de_joueurs).

test(det) :-
   assertion( aggregate_all(count, nombre_de_joueurs(_), 1) ).

test(au_moins_trois) :-
   nombre_de_joueurs(N_J),
   assertion( N_J >= 3 ).

:- end_tests(nombre_de_joueurs).

%! joueurs(?JS: [joueur(atom, atom, term)]) is det.
%
% Vue en extension du prédicat 'joueur/3'.
%
% @arg JS   La liste de tous les joueurs connus avec leur propriétés associées : leur stratégie de jeu et l'état initial de leur cerveau
%
% @throws Postcondition.   La longueur de la liste est égale au nombre de joueurs.
%
joueurs(JS) :-
   findall( joueur(J, S, B)
          , joueur(J, S, B)
          , JS
          ).

:- begin_tests(joueurs).

test(det) :-
   assertion( aggregate_all(count, joueurs(_), 1) ).

test(longueur_egale_nombre) :-
   nombre_de_joueurs(N_J),
   joueurs(JS),
   assertion( length(JS, N_J) ).

:- end_tests(joueurs).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% DISTRIBUTION
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%! nombre_de_cartes_initiales_par_joueur(?N: nat) is det.
%
% Le nombre de cartes devant être distribuées à chaque joueur en début de manche.
%
% @arg N   Un nombre de cartes
%
% @throws Postcondition.   Le nombre de cartes est strictement positif.
% @throws Postcondition.   Le nombre de cartes par le nombre de joueurs ne doit pas excéder 50.
%
nombre_de_cartes_initiales_par_joueur(7).

:- begin_tests(nombre_de_cartes_initiales_par_joueur).

test(det) :-
   assertion( aggregate_all(count, nombre_de_cartes_initiales_par_joueur(_), 1) ).

test(strictement_positif) :-
   nombre_de_cartes_initiales_par_joueur(N),
   assertion( N > 0 ).

test(assez_de_cartes) :-
   nombre_de_joueurs(N_J),
   nombre_de_cartes_initiales_par_joueur(N_C),
   assertion( ( multiply(N_J, N_C, N)
              , N =< 50
              ) ).

:- end_tests(nombre_de_cartes_initiales_par_joueur).

%! distribution(+J: [carte], -MS: [[carte]], -P3: piles) is det.
%
% Distribution des cartes en début de manche entre les joueurs et sur les trois piles.
%
% @arg J    Un jeu de cartes à battre et distribuer
% @arg MS   Les mains de chaque joueur
% @arg P3   Les trois piles de carte, les deux premières visibles et "multipiles", la troisième constituant la pioche, aux cartes invisibles
%
% @throws Précondition.   Les cartes constituent un jeu.
%
% @throws Postcondition.   Il y a le nombre de joueurs indiqué.
% @throws Postcondition.   Chaque main des joueurs contient le nombre de cartes initiales.
% @throws Postcondition.   La première pile visible contient une unique carte.
% @throws Postcondition.   La deuxième pile visible contient une unique carte.
% @throws Postcondition.   L'ensemble des cartes, distribuées aux joueurs et mises sur les piles, constitué le jeu d'origine.
%
distribution(J, _, _) :-
   assertion( nonvar(J) ),
   sort(J, J_sorted),
   \+ jeu(J_sorted),
   format("Precondition 'distribution(~w, _, _)' violated:  cards are missing in the game\n", [J]),
   assertion( false ).
%
distribution(J, MS, piles([[C1]], [[C2]], CS)) :-
   manual_random_shuffle(J, J_shuffled),
   nombre_de_joueurs(N_J),
   nombre_de_cartes_initiales_par_joueur(N_C),
   multiply(N_J, N_C, N),
   take(N, J_shuffled, J_joueurs),
   interleave(J_joueurs, N_J, MS),
   drop(N, J_shuffled, [C1, C2 | CS]).

:- begin_tests(distribution).

test(det) :-
   jeu(J),
   assertion( forall( between(1, 100, _)
                    , aggregate_all(count, distribution(J, _, _), 1)
                    ) ).

test(nombre_de_joueurs) :-
   jeu(J),
   nombre_de_joueurs(N_J),
   assertion( forall( ( between(1, 100, _)
                      , distribution(J, MS, _)
                      )
                    , length(MS, N_J)
                    ) ).

test(cartes_en_mains) :-
   jeu(J),
   nombre_de_cartes_initiales_par_joueur(N_C),
   assertion( forall( ( between(1, 100, _)
                      , distribution(J, MS, _)
                      )
                    , forall( member(M, MS)
                            , length(M, N_C)
                            )
                    ) ).

test(pile_visible_1_une_carte) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , distribution(J, _, (P1, _, _))
                      )
                    , P1 = [[_]]
                    ) ).

test(pile_visible_2_une_carte) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , distribution(J, _, (_, P2, _))
                      )
                    , P2 = [[_]]
                    ) ).

test(jeu_complet) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , distribution(J, MS, ([[C1]], [[C2]], PS))
                      )
                    , ( append(MS, MCS)
                      , append(MCS, [C1, C2 | PS], J_distribue)
                      , sort(J_distribue, J_sorted)
                      , jeu(J_sorted)
                      )
                    ) ).

:- end_tests(distribution).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PLI
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Improprement appelé un pli, il s'agit ici de faire jouer un joueur.
% Le "pli", se décompose en trois opérations successives :
%
%    1. Tout d'abord, s'il en a la possibilité, le joueur peut annoncer "moins de neuf", ce qui déclenche la phase finale de la manche.
%
%    2. Autrement, il peut commencer par se défausser d'une combinaison en main, s'il en a une bien entendu mais aussi s'il le souhaite, ou encore, ou alors, d'une unique carte
%
%    3. Enfin, il doit obligatoirement prélever une carte, soit parmi celles visibles soit dans la pioche.
%
% Le respect des règles est assuré par les prédicats respectifs :
%
%    1. 'annonce/7',
%
%    2. 'defausse/7' et
%
%    3. 'pioche/7'.
%
% En préalable, ils vérifient si ces coups sont possibles pour le joueur en fonction de sa main et des cartes sur la table.
% Lorsque les possibilités existent, ils invoquent respectivement les prédicats permettant aux "Intelligences artificielles" de prendre leur décision, soit :
%
%    1. 'annonce_strategique/7',
%
%    2. 'defausse_strategique/8' et
%
%    3. 'pioche_strategique/7'.
%
% A posteriori, le code joue encore le rôle d'arbitre en vérifiant que les "Intelligences artificielles" n'ont pas triché.
%
% La communication entre ces prédicats, pendant une manche et pendant une partie se, font au travers de leurs "cerveaux".
% Il s'agit de variables qui peuvent être perçus comme des "variables globales" par les "Intelligences artificielles", le programme se chargeant de faire circuler les valeurs.
% Le code vérifie également que les cerveaux ne se perdent pas en cours de réflexion !
% Par ailleurs, toutes les informations visibles, cartes posées et prélevées par les autres joueurs, sont également transmises aux "Intelligences artificielles" dans les arguments des prédicats.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%! strategie(+S: atom) is semidet.
%! strategie(-S: atom) is nondet.
%
% Les stratégies de jeu disponibles.
%
% @arg S   Une stratégie de jeu
%
% @throws Postcondition.   Les stratégies sont clé de la relation.
% @throws Postcondition.   Tous les joueurs doivent avoir choisi une stratégie effectivement disponible.
%
strategie(primitive).
strategie(aleatoire).
strategie(gloutonne).
strategie(humaine).

:- begin_tests(strategie).

test(cle) :-
   assertion( forall( ( strategie(S)
                      , findall(S, strategie(S), SS)
                      )
                    , length(SS, 1)
                    ) ).

test(strategie_joueur) :-
   assertion( forall( joueur(_, S, _)
                    , strategie(S)
                    ) ).

:- end_tests(strategie).

%! piles_defausse_possible(+T3: sommets, -PS: [{ pile_1, pile_2}]) is det.
%
% Pour éviter des blocages entre la défausse et la pioche qui suit, le choix de la pile visible où poser la défausse, s'impose parfois.
%
% @arg T3   Les trois sommets de piles sur la table
% @arg PS   L'unique pile visible qui évite de bloquer la pioche, ou alors les deux les piles visibles s'il n'y a pas de problème
%
% @throws Precondition.   Les trois piles en peuvent pas être simultanément vides.
%
% @throws Postcondition.   Il y a toujours une pile où poser la défausse.
%
piles_defausse_possible(T3, _) :-
   sommets_vides(T3),
   format("Precondition 'piles_defausse_possible(~w, _)' violated:  all stacks are empty\n", [T3]),
   assertion( false ).
%
piles_defausse_possible(sommets([], _, 0), [pile_1]).
piles_defausse_possible(sommets(_, [], 0), [pile_2]).
piles_defausse_possible(sommets(_, _, N_P), [pile_1, pile_2]) :-
   N_P > 0.
piles_defausse_possible(sommets([_ | _], [_ | _], 0), [pile_1, pile_2]).

:- begin_tests(piles_defausse_possible).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 1, P1_max)
                      , random_between(0, 1, P2_max)
                      , random_between(0, 1, P_max) % 25 % de cas avec une seule pile
                      , random_sommets(J, (P1_max, P2_max, P_max), T3)
                      , \+ sommets_vides(T3)
                      )
                    , aggregate_all(count, piles_defausse_possible(T3, _), 1)
                    ) ).

test(au_moins_une_pile) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , random_between(0, 1, P1_max)
                      , random_between(0, 1, P2_max)
                      , random_between(0, 1, P_max)
                      , random_sommets(J, (P1_max, P2_max, P_max), T3)
                      , \+ sommets_vides(T3)
                      )
                    , piles_defausse_possible(T3, [_ | _])
                    ) ).

:- end_tests(piles_defausse_possible).

%! defausse_strategique(+S: atom, +PS: [pli], +M: [carte], +CSS: [[carte]], +T3: sommets, +B, -D: defausse, -B_post) is det.
%
% Strategies disponibles.
%
% @arg S        Une stratégie de jeu
% @arg PS       Les plis précédents du tour de jeu
% @arg M        La main
% @arg CSS      Les combinaisons en main
% @arg T3       Les trois sommets de piles sur la table
% @arg B        L'état du cerveau ("brain") du joueur
% @arg D        La défausse est (CS, P), c'est-à-dire *les* cartes posées, éventuellement une liste en contenant une seule, et le numéro de la pile visible sur laquelle elles sont posées
% @arg B_post   L'état du cerveau ("brain") du joueur après avoir réfléchi
%
% @throws Precondition.   La main n'est pas vide.
%
% @throws Postcondition.   Toutes les stratégies doivent avoir une règle de défausse.
% @throws Postcondition.   La défausse est l'une des combinaisons en main ou une carte en main.
% @throws Postcondition.   Le numéro de pile visible est 1 ou 2, seulement si l'ensemble des cartes posées est non vide.
% @throws Postcondition.   Le numéro de pile visible s'impose si l'une des piles visibles est vide ainsi que la pioche : il s'agit de se défausser sur la pile visible vide pour pouvoir piocher dans l'autre pile visible.
%
defausse_strategique(_, _, [], _, _, _, _, _) :-
   format("Precondition 'defausse_strategique(_, _, [], _, _, _, _, _)' violated:  empty hand\n", []),
   assertion( false ).
%
% stratégie primitive
%
defausse_strategique(primitive, _, _, [CS | _], T3, B, defausse(CS, P), B) :-  % La stratégie primitive pose la première combinaison qui se présente, toujours sur la première pile possible, sans réfléchir plus avant.
   assertion( (nonvar(CS), nonvar(T3)) ),
   piles_defausse_possible(T3, [P | _]),
   !.
defausse_strategique(primitive, _, [C | _], _, T3, B, defausse([C], P), B) :-  % À défaut de combinaison, elle pose la première carte...
   assertion( nonvar(C) ),
   piles_defausse_possible(T3, [P | _]).
%
% stratégie aléatoire
%
defausse_strategique(aleatoire, _, M, [], T3, B, defausse([C], P), B) :-  % À défaut de combinaison, la stratégie aléatoire
   assertion( nonvar(M) ),
   random_member(C, M),                                                   % choisit une carte quelconque dans sa main
   piles_defausse_possible(T3, PS),
   random_member(P, PS),                                                  % et la pose sur l'une ou l'autre pile visible, et autorisée, de manière équiprobable également.
   !.
defausse_strategique(aleatoire, _, _, CSS, T3, B, defausse(CS, P), B) :-  % En revanche, si elle a une combinaison,
   assertion( nonvar(CSS) ),
   length(CSS, N),
   random_between(1, N, I),                                               % avec une probabilité égale,
   nth1(I, CSS, CS),                                                      % l'une des combinaisons disponibles est choisie,
   piles_defausse_possible(T3, PS),
   random_member(P, PS).                                                  % et posée sur l'une ou l'autre pile visible de manière équiprobable également.
%
% stratégie gloutonne
%
defausse_strategique(gloutonne, _, M, [], T3, cerveau_glouton(B1, B2, B3), defausse([C_max], P), cerveau_glouton(B1_plus_1, B2, B3)) :-  % Quant à la stratégie gloutonne, en l'absence de combinaison, et seulement en l'absence de combinaison,
   findall((P, C), (member(C, M), points_cartes(C, P)), CPS),                 % après avoir calculé le nombre de points de chaque carte présente dans la main,
   argmax_list(C_max, CPS),                                                   % elle sélectionne l'une de celles permettant de se débarrasser d'un maximum de points,
   !,                                                                         % et une seule,
   piles_defausse_possible(T3, PS),
   random_member(P, PS),                                                      % et de la poser sur l'une ou l'autre pile visible de manière équiprobable également.
   plus(B1, 1, B1_plus_1).
defausse_strategique(gloutonne, _, _, CSS, T3, cerveau_glouton(B1, B2, B3), defausse(CS_max, P), cerveau_glouton(B1_plus_1, B2, B3)) :-  % Si la stratégie gloutonne a des combinaisons en main,
   assertion( nonvar(CSS) ),
   assertion( ( nonvar(B1), nonvar(B2), nonvar(B3)) ),
   CSS \= [],
   findall((P, CS), (member(CS, CSS), points_cartes(CS, P)), KPS),            % après avoir calculé le nombre de points de chaque combinaison présente dans la main,
   % maplist([CS, (P, CS)]>>points_cartes(CS, P), CSS, KPS),                  % (la version avec lambda se révélant trop coûteuse),
   argmax_list(CS_max, KPS),                                                  % elle sélectionne l'une de celles permettant de poser un maximum de points,
   !,                                                                         % et une seule,
   piles_defausse_possible(T3, PS),
   random_member(P, PS),                                                      % et de la poser sur l'une ou l'autre pile visible de manière équiprobable également.
   plus(B1, 1, B1_plus_1).
%
% stratégie humaine
%
defausse_strategique(humaine, PS, M, CSS, T3, B, defausse(CS, P), B) :-  % Enfin, pour la stratégie humaine,
   assertion( (nonvar(PS), nonvar(M), nonvar(CSS), nonvar(T3), nonvar(B)) ),
   presenter_jeu(PS, M, CSS, T3),                                        % il suffit de présenter l'état du jeu
   demander_cartes_defausse(M, CSS, CS),                                 % et de demander.
   demander_pile(T3, P).

%! presenter_jeu(+PS, +M, +CSS, +T3) is det.
%
presenter_jeu(PS, M, CSS, T3) :-
   assertion( (nonvar(PS), nonvar(M), nonvar(CSS), nonvar(T3)) ),
   presenter_plis_precedents(PS),
   presenter_sommets(T3),
   presenter_main(M),
   presenter_combinaisons(CSS).

%! presenter_plis_precedents(+PS) is det.
%
presenter_plis_precedents([]) :-
   format("Aucun pli encore\n", []).
presenter_plis_precedents(PS) :-
   assertion( nonvar(PS) ),
   PS \= [],
   format("Plis précédents :\n", []),
   forall( member(P, PS)
         , ( pretty_pli(P, T_P)
           , format("   - ~w\n", [T_P])
           )
         ).

%! pretty_pli(+P, -T_P) is det.
%
pretty_pli(P, T_P) :-
   assertion( nonvar(P) ),
   ( P = 'moins de neuf'                         -> T_P = 'moins de neuf'
   ; P = pli('aucune defausse', 'aucune pioche') -> T_P = passe  % Pourrait se produire dans une variante des règles
   ; P = pli('aucune defausse', pioche)          -> T_P = pioche
   ; P = pli('aucune defausse', C)               -> ( pretty_carte(C, T_C)
                                                    , format_to_chars("ramasse ~w", [T_C], T_inter)
                                                    , atom_chars(T_P, T_inter)
                                                    )
   ; P = pli(defausse(K, _), pioche)            -> ( cartes_combinaison(CS, K)
                                                   , pretty_cartes(CS, T_CS)
                                                   , format_to_chars("pose ~w et pioche", [T_CS], T_inter)
                                                   , atom_chars(T_P, T_inter)
                                                   )
   ; P = pli(defausse(K, _), C)                 -> ( cartes_combinaison(CS, K)
                                                   , pretty_cartes(CS, T_CS)
                                                   , pretty_carte(C, T_C)
                                                   , format_to_chars("pose ~w et ramasse ~w", [T_CS, T_C], T_inter)
                                                   , atom_chars(T_P, T_inter)
                                                   )
   ;                                               ( format("pretty_pli(~w). CAS IMPRÉVU\n", [P])
                                                   , assertion( false )
                                                   )
   ).

%! presenter_main(+M) is det.
%
presenter_main([]) :-
   format("Main vide !\n", []).
presenter_main(M) :-
   assertion( nonvar(M) ),
   M \= [],
   pretty_cartes(M, T_M),
   format("Main : ~w\n", [T_M]).

%! presenter_combinaisons(+CSS) is det.
%
presenter_combinaisons([]) :-
   format("Aucune combinaisons en main...\n", []).
presenter_combinaisons(CSS) :-
   assertion( nonvar(CSS) ),
   CSS \= [], 
   format("Combinaisons en main :\n", []),
   length(CSS, N),
   forall( between(1, N, J)
         , ( nth1(J, CSS, K)
           , cartes_combinaison(CS, K)
           , sort(CS, CS_sorted)
           , pretty_cartes(CS_sorted, T_CS)
           , format("   ~w. ~w\n", [J, T_CS])
           )
         ).

%! presenter_sommets(+T3) is det.
%
presenter_sommets(sommets(T1, T2, N_P)) :-
   assertion( (nonvar(T1), nonvar(T2), nonvar(N_P)) ),
   pretty_cartes(T1, T_T1),
   pretty_cartes(T2, T_T2),
   format("Pile 1 : ~w\n", [T_T1]),
   format("Pioche : ~w cartes\n", [N_P]),
   format("Pile 2 : ~w\n", [T_T2]).

%! demander_cartes_defausse(+N, +CSS, -CS) is det.
%
demander_cartes_defausse(M, CSS, CS) :-
   assertion( (nonvar(M), nonvar(CSS)) ),
   length(M, N_M),
   length(CSS, N_CSS),
   format("Quelle carte (n° entre -1 et -~w) ou combinaison (n° entre 1 et ~w) posez-vous ?\n", [N_M, N_CSS]),
   current_input(Stdin),
   read_line_to_string(Stdin, S),
   string_to_atom(S, A),
   atom_number(A, K),
   N_M_minus is - N_M,
   ( between(N_M_minus, -1, K) -> ( K_minus is - K
                                  , nth1(K_minus, M, C)
                                  , CS = [C]
                                  )
   ; between(1, N_CSS, K)      -> nth1(K, CSS, CS)
   ;                              demander_cartes_defausse(M, CSS, K)
   ).

%! demander_pile(+P) is det.
%
demander_pile(T3, P) :-
   piles_defausse_possible(T3, [P]),
   format("Vous ne pouvez poser que sur la pile ~w !\n", [P]),
   !.
demander_pile(_, P) :-
   assertion( var(P) ),
   format("Sur quelle pile visible la posez-vous ? (1 ou 2)\n", []),
   current_input(Stdin),
   read_line_to_string(Stdin, K),
   ( K = "1" -> P = pile_1
   ; K = "2" -> P = pile_2
   ;            demander_pile(P)
   ).

:- begin_tests(defausse_strategique).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_non_empty_random_subset_of_at_most(M, J, 11)
                      , combinaisons(M, CSS)
                      , strategie(S)
                      , S \= humaine
                      , random_sommets(J, T3)
                      , \+ sommets_vides(T3)
                      , cerveau_defaut(S, B)
                      )
                    , aggregate_all(count, defausse_strategique(S, [], M, CSS, T3, B, _, _), 1)
                    ) ).

test(combinaison_existante_ou_carte) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_non_empty_random_subset_of_at_most(M, J, 11)
                      , combinaisons(M, CSS)
                      , strategie(S)
                      , S \= humaine
                      , random_sommets(J, T3)
                      , \+ sommets_vides(T3)
                      , cerveau_defaut(S, B)
                      , defausse_strategique(S, [], M, CSS, T3, B, D, _)
                      )
                    , ( D = defausse([C], _) -> member(C, M)
                      ; D = defausse(CS, _)  -> member(CS, CSS) 
                      )
                    ) ).

test(pile_existante_si_combinaison_existante) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_non_empty_random_subset_of_at_most(M, J, 11)
                      , combinaisons(M, CSS)
                      , strategie(S)
                      , S \= humaine
                      , random_sommets(J, T3)
                      , \+ sommets_vides(T3)
                      , cerveau_defaut(S, B)
                      , defausse_strategique(S, [], M, CSS, T3, B, defausse(_, P), _)
                      )
                    , member(P, [pile_1, pile_2])
                    ) ).

:- end_tests(defausse_strategique).

%! defausse(+S: atom, +PS: [pli], +M: [carte], +T3: sommets, +B, -D: defausse, -B_post) is det.
%
% La première étape lorsqu'un joueur joue est, éventuellement, de se défausser de certaines de ces cartes, celles pouvant former une combinaison.
%
% @arg S        Une stratégie de jeu
% @arg PS       Les quelques plis précédents
% @arg M        La main du joueur
% @arg T3       Les trois sommets de piles de cartes sur la table
% @arg B        L'état du cerveau (brain) du joueur
% @arg D        La défausse est soit "defausse(CS, P)", c'est-à-dire la ou les cartes posées et le numéro de la pile visible sur laquelle elles sont posées, soit 'aucune defausse'
% @arg B_post   L'état du cerveau (brain) du joueur après avoir réfléchi
%
% @throws Precondition.   Le nombre de plis précédents est au maximum égal au nombre de joueurs.
% @throws Precondition.   La main n'est pas vide.
%
% @throws Postcondition.   Toutes les stratégies doivent avoir une règle de défausse.
% @throws Postcondition.   La défausse doit être une combinaison présente dans la main ou une carte en main, ou encore 'aucune defausse' mais si, et seulement si, la main est vide.
% @throws Postcondition.   Le numéro de pile visible est 1 ou 2, seulement si l'ensemble des cartes posées est non vide.
% @throws Postcondition.   Le numéro de pile visible s'impose si l'une des piles visibles est vide ainsi que la pioche : il s'agit de se défausser sur la pile visible vide pour pouvoir piocher dans l'autre pile visible.
%
defausse(_, PS, _, _, _, _, _) :-
   assertion( nonvar(PS) ),
   length(PS, N_PS),
   joueurs(JS),
   length(JS, N_JS),
   N_PS > N_JS,
   format("Precondition 'defausse(_, ~w, _, _, _, _, _)' violated:  too many playing tours\n", [PS]),
   assertion( false ).
defausse(_, _, [], _, _, _, _) :-
   format("Precondition 'defausse(_, _, [], _, _, _, _, _)' violated:  empty hand\n", []),
   assertion( false ).
%
defausse(S, PS, M, T3, B, D, B_post) :-                         % Pour n'importe quelle stratégie,
   assertion( (nonvar(S), nonvar(M), nonvar(T3), nonvar(B)) ),
   combinaisons(M, CSS),                                        % on précalcule les combinaisons,
   defausse_strategique(S, PS, M, CSS, T3, B, D, B_post),       % mais c'est à la stratégie particulière de décider.
   !,                                                           % (Ici, on assure qu'il n'y aura pas de point de retour dû à une décision incomplète de l'agent.)
   defausse_sans_triche(S, PS, M, CSS, T3, B, D, B_post).       % Puis l'arbitre vérifie qu'elle ne triche pas... et pourra continuer à réfléchir !

defausse_sans_triche(S, PS, M, CSS, T3, B, D, B_post) :-
   ( D = 'aucune defausse'       -> true
   ; D = defausse([C], P),
     member(C, M),
     member(P, [pile_1, pile_2]) -> true
   ; D = defausse(CS, P),
     member(CS, CSS),
     member(P, [pile_1, pile_2]) -> true
   ; true                        -> format("La stratégie ~w a triché durant la défausse ! PS : ~w, M : ~w, CSS : ~w, T3 : ~w, B : ~w, D : ~w\n", [S, PS, M, CSS, T3, B, D]),
                                    assertion( false )
   ),
   ( sommets_pioche(T3, D, T3_pioche),
     pioche_imposee(T3_pioche, 'aucune pioche') -> format("La stratégie ~w s'est mise en position ne de pas pouvoir piocher ! PS : ~w, M : ~w, CSS : ~w, T3 : ~w, B : ~w, D : ~w\n", [S, PS, M, CSS, T3, B, D]),
                                                   assertion( false )
   ; true
   ),
   ( nonvar(B_post) -> true
   ; true           -> format("La stratégie ~w a perdu son cerveau durant la défausse ! PS : ~w, M : ~w, CSS : ~w, T3 : ~w, B : ~w, D : ~w\n", [S, PS, M, CSS, T3, B, D]),
                       assertion( false )
   ).

:- begin_tests(defausse).

test(det_strategie) :-
   jeu(J),
   assertion( forall( ( between(1, 50, _)
                      , is_non_empty_random_subset_of_at_most(M, J, 11)
                      , strategie(S)
                      , S \= humaine
                      , random_sommets(J, T3)
                      , \+ sommets_vides(T3)
                      , cerveau_defaut(S, B)
                      )
                    , defausse(S, [], M, T3, B, _, _)
                    ) ).

test(combinaison_ou_carte_existante_ou_aucune_defausse) :-
   jeu(J),
   assertion( forall( ( between(1, 50, _)
                      , is_non_empty_random_subset_of_at_most(M, J, 11)
                      , strategie(S)
                      , S \= humaine
                      , random_sommets(J, T3)
                      , \+ sommets_vides(T3)
                      , cerveau_defaut(S, B)
                      , defausse(S, [], M, T3, B, D, _)
                      )
                    , ( D = 'aucune defausse' -> true
                      ; D = defausse([C], _)  -> member(C, M)
                      ; D = defausse(CS, _)   -> combinaison(M, CS)
                      )
                    ) ).

test(pile_existante_si_carte) :-
   jeu(J),
   assertion( forall( ( between(1, 50, _)
                      , is_non_empty_random_subset_of_at_most(M, J, 11)
                      , strategie(S)
                      , S \= humaine
                      , random_sommets(J, T3)
                      , \+ sommets_vides(T3)
                      , cerveau_defaut(S, B)
                      , defausse(S, [], M, T3, B, D, _)
                      )
                    , ( D = 'aucune defausse'
                      ; ( D = defausse(_, P)
                        , member(P, [pile_1, pile_2])
                        )
                      )
                    ) ).

test(pile_imposee) :-
   jeu(J),
   assertion( forall( ( between(1, 50, _)
                      , is_non_empty_random_subset_of_at_most(M, J, 11)
                      , strategie(S)
                      , S \= humaine
                      , random_sommets(J, T3)
                      , \+ sommets_vides(T3)
                      , defausse(S, [], M, T3, cerveau_vide, D, _)
                      )
                    , ( T3 = sommets(_, [], 0) -> D = defausse(_, pile_2)
                      ; T3 = sommets([], _, 0) -> D = defausse(_, pile_1)
                      ; true
                      )
                    ) ).

:- end_tests(defausse).

%! pioche_imposee(+T3: sommets, -C: { carte, 'pioche' }) is semidet.
%
% Dans certaines situations, très rares, la carte à prendre dans l'un des sommets est unique.
%
% @arg T3   Les trois sommets de cartes sur la table
% @arg C    La carte à ramasser, unique
%
% @throws Postcondition.   La pioche est imposée si, et seulement si, la somme des nombres de cartes au sommet des sommets est égale à un, la pioche valant zéro ou un.
%
pioche_imposee(sommets([],  [], 0  ), 'aucune pioche').   % Une pseudo pioche est imposée s'il n'y a plus aucune carte sur la table...
pioche_imposee(sommets([C], [], 0  ), C) :-               % La pioche est imposée s'il n'y a qu'une unique carte disponible au sommet de la première pile visible.
   assertion( nonvar(C) ).
pioche_imposee(sommets([], [C], 0  ), C) :-               % Il en va de même pour une unique carte disponible au sommet de la deuxième pile visible.
   assertion( nonvar(C) ).
pioche_imposee(sommets([],  [], N_P), pioche) :-          % Enfin, la pioche est imposée s'il n'y a aucune carte visible,
   assertion( nonvar(N_P) ),
   N_P > 0.                                               % mais que la pioche n'est pas épuisée.

:- begin_tests(pioche_imposee).

test(semidet) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(CS, J)
                      , random_sommets(CS, T3)
                      , \+ sommets_vides(T3)
                      )
                    , ( aggregate_all(count, pioche_imposee(T3, _), N)
                      , between(0, 1, N)
                      )
                    ) ).

test(imposee_ssi_choix_unique) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of(CS, J)
                      , random_sommets(CS, T3)
                      , \+ sommets_vides(T3)
                      )
                    , ( T3 = sommets(T1, T2, N_P)
                      , length(T1, N1)
                      , length(T2, N2)
                      , plus(N1, N2, N12)
                      , N_P_bin is min(N_P, 1)
                      , plus(N12, N_P_bin, N)
                      , ( N = 1 ->    pioche_imposee(T3, _)
                        ; N > 1 -> \+ pioche_imposee(T3, _)
                        )
                      )
                    ) ).

:- end_tests(pioche_imposee).

%! pioche_strategique(+S: atom, +M: [carte], +T3: sommets, +B, -C: { carte, 'pioche' }, -B_post) is det.
%
% Chaque "Intelligence artificielle" (ou joueur humain) réfléchit ici à la meilleure pioche, le choix étant garanti.
%
% @arg S        Une stratégie de jeu
% @arg PS       Les plis précédents
% @arg M        La main
% @arg T3       Les trois sommets de pile sur la table
% @arg B        L'état du cerveau (brain) du joueur
% @arg C        La carte ramassée, éventuellement dans la pioche
% @arg B_post   L'état du cerveau (brain) du joueur après avoir réfléchi
%
% @throws Precondition.   Au moins deux sommets sont ouverts au choix, c'est-à-dire que la pioche n'est pas imposée.
%
% @throws Postcondition.   Toutes les stratégies doivent avoir une règle de défausse.
% @throws Postcondition.   La carte est ramassée est soit sur un sommets visible soit la pioche.
%
% @bug  La stratégie gloutonne doit être modifiée si les valeurs des cartes le sont.
%
pioche_strategique(_, _, _, T3, _, _, _) :-
   assertion( nonvar(T3) ),
   pioche_imposee(T3, _),
   format("Precondition 'pioche_strategique(_, _, _, ~w, _, _, _)' violated:  mandatory choice\n", [T3]),
   assertion( false ).
%
% stratégie primitive
%
pioche_strategique(primitive, _, _, sommets([C | _], _, _), B, C, B).   % La stratégie primitive prend toujours la première carte qui se présente, sans réfléchir,
pioche_strategique(primitive, _, _, sommets([], [C | _], _), B, C, B).  % tout d'abord dans les piles visibles
pioche_strategique(primitive, _, _, sommets([], [], _), B, pioche, B).  % puis dans la pioche.
%
% stratégie aléatoire
%
pioche_strategique(aleatoire, _, _, sommets(CS1, CS2, 0), B, C, B) :-    % La stratégie aléatoire choisit, quand la pioche est vide,
   append([CS1, CS2], CS),
   random_member(C, CS).                                                 % de manière équiprobable l'une des cartes visibles.
pioche_strategique(aleatoire, _, _, sommets(CS1, CS2, N_P), B, C, B) :-
   N_P > 0,                                                              % Si la pioche est disponible,
   append([CS1, CS2, [pioche]], CS),
   random_member(C, CS).                                                 % on y ajoute une chance de piocher une carte cachée.
%
% stratégie gloutonne
%
pioche_strategique(gloutonne, _, _, sommets(T1, T2, 0), cerveau_glouton(B1, B2, B3), C, cerveau_glouton(B1, B2_plus_1, B3)) :-    % Pour ce qui est de la stratégie gloutonne, si la pioche est vide,
   assertion( ( nonvar(B1), nonvar(B2), nonvar(B3)) ),
   append(T1, T2, CS),
   findall((V, K), (member(K, CS), carte(K, V, _)), VCS),              % après avoir calculé la valeur de chaque carte visible,
   % maplist([K, (V, K)]>>carte(K, V, _), CS, VCS),
   argmin_list(C, VCS),                                                % elle sélectionne une seule de celles permettant de prendre un minimum de points,
   !,                                                                  % et une seule.
   plus(B2, 1, B2_plus_1).
pioche_strategique(gloutonne, _, _, sommets(T1, T2, N_P), cerveau_glouton(B1, B2, B3), C, cerveau_glouton(B1, B2_plus_1, B3)) :-  % Mais,
   N_P > 0,                                                            % si la pioche n'est pas vide,
   append(T1, T2, CS),
   findall((V, K), (member(K, CS), carte(K, V, _)), VCS),              % après avoir calculé la valeur de chaque carte visible,
   % maplist([K, (V, K)]>>carte(K, V, _), CS, VCS),
   argmin_list(C_min, VCS),                                            % es sélectionné une,
   !,                                                                  % et une seule, de celles
   carte(C_min, V_min, _),                                             % de valeur minimale,
   ( V_min =< 7 -> C = C_min                                           % si la valeur de cette dernière est inférieure ou égale à sept, alors on la garde,
   ; V_min >  7 -> C = pioche                                          % sinon on pioche car la probabilité de prendre plus petit a dépassé la moyenne.
   ),
   plus(B2, 1, B2_plus_1).
%
% stratégie humaine
%
pioche_strategique(humaine, PS, M, T3, B, C, B) :-  % Enfin, pour la stratégie humaine,
   assertion( (nonvar(PS), nonvar(M), nonvar(T3), nonvar(B)) ),
   presenter_main(M),                               % après avoir rappelé les cartes en main,
   presenter_cartes_ramassables(T3, N_min, N_max),  % il suffit de présenter les possibilités
   demander_carte(N_min, N_max, K),                 % de demander laquelle est choisie,
   T3 = sommets(T1, T2, _),                         % puis de retrouver la carte correspondante, ou alors 'pioche', la valeur étant retrouvée par l'arbitre.
   length(T1, N1),
   plus(N1, J, K),
   ( K = 0   -> C = pioche
   ; K =< N1 -> nth1(K, T1, C)
   ; K >  N1 -> nth1(J, T2, C)
   ).

%! presenter_cartes_ramassables(+T3, -N_min, -N_max) is det.
%
presenter_cartes_ramassables(T3, _, _) :-
   assertion( nonvar(T3) ),
   sommets_vides(T3),
   format("Aucune carte ramassable !\n", []),
   !.
presenter_cartes_ramassables(sommets(T1, T2, N_P), N_min, N_max) :-
   format("Cartes ramassables :\n", []),
   ( N_P =< 0 -> N_min = 1
   ; N_P >  0 -> N_min = 0,
                 format("   0. pioche\n", [])
   ),
   append(T1, T2, T),
   length(T, N_max),
   forall( between(1, N_max, J)
         , ( nth1(J, T, C)
           , pretty_carte(C, T_C)
           , format("   ~w. ~w\n", [J, T_C])
           )
         ).

%! demander_carte(+N_min, +N_max, -K) is det.
%
demander_carte(N_min, N_max, K) :-
   assertion( (nonvar(N_min), nonvar(N_max)) ),
   assertion( N_min >= 0 ),
   assertion( N_min =< N_max ),
   format("Quelle carte ramassez-vous ? (n° entre ~w et ~w)\n", [N_min, N_max]),
   current_input(Stdin),
   read_line_to_string(Stdin, S),
   string_to_atom(S, A),
   atom_number(A, K),
   between(N_min, N_max, K),
   !.
demander_carte(N_min, N_max, K) :-
   demander_carte(N_min, N_max, K).

:- begin_tests(pioche_strategique).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of_at_most(M, J, 11)
                      , subtract(J, M, JR)
                      , random_sommets(JR, T3)
                      , \+ pioche_imposee(T3, _)
                      , strategie(S)
                      , S \= humaine
                      , cerveau_defaut(S, B)
                      )
                    , aggregate_all(count, pioche_strategique(S, M, [], T3, B, _, _), 1)
                    ) ).

test(carte_existante_ou_pioche) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , is_random_subset_of_at_most(M, J, 11)
                      , subtract(J, M, JR)
                      , random_sommets(JR, T3)
                      , \+ pioche_imposee(T3, _)
                      , strategie(S)
                      , S \= humaine
                      , cerveau_defaut(S, B)
                      , pioche_strategique(S, M, [], T3, B, C, _)
                      , sommets(T1, T2, _) = T3
                      )
                    , ( member(C, T1)
                      ; member(C, T2)
                      ; C = pioche
                      )
                    ) ).

:- end_tests(pioche_strategique).

%! pioche(+S: atom, +PS: [pli], +M: [carte], +T3: sommets, +B, -C: pioche, -B_post) is det.
%
% La deuxième étape lorsqu'un joueur joue est de piocher une carte dans l'un des trois sommets.
%
% @arg S        Une stratégie de jeu
% @arg PS       Les plis précédents
% @arg M        La main du joueur
% @arg T3       Les trois sommets de cartes sur la table
% @arg B        L'état du cerveau ("brain") du joueur
% @arg C        La carte piochée, éventuellement inconnue
% @arg B_post   L'état du cerveau ("brain") du joueur après avoir réfléchi
%
% @throws Postcondition.   Il faut pouvoir piocher.
%
% @throws Postcondition.   Toutes les stratégies doivent avoir une règle de défausse.
% @throws Postcondition.   La défausse doit être une combinaison présente dans la main.
% @throws Postcondition.   Le numéro de pile visible est 1 ou 2, seulement si l'ensemble des cartes posées est non vide.
%
pioche(_, _, _, T3, _, _, _) :-
   assertion( nonvar(T3) ),
   pioche_imposee(T3, 'aucune pioche'),
   format("Precondition 'pioche(_, _, _, T3, _, _, _)' violated:  aucune pioche possible\n", [T3]),
   assertion( false ).
%
pioche(_, _, _, T3, B, C, B) :-
   assertion( (nonvar(T3), nonvar(B)) ),
   pioche_imposee(T3, C),                           % Si le choix est fermé (pas impossible), on prélève la seule carte disponible, sans avoir à réfléchir,
   !.                                               % Sinon
pioche(S, PS, M, T3, B, C, B_post) :-
   assertion( (nonvar(S), nonvar(PS), nonvar(M)) ),
   assertion( \+ pioche_imposee(T3, _) ),
   pioche_strategique(S, PS, M, T3, B, C, B_post),  % c'est à la stratégie de décider.
   !,                                               % (Ici, on assure qu'il n'y aura pas de point de retour dû à une décision incomplète de l'agent.)
   pioche_sans_triche(S, PS, M, T3, B, C, B_post).  % Mais c'est à l'arbitre de vérifier qu'elle ne triche pas... et pourra continuer à réfléchir !

pioche_sans_triche(S, PS, M, T3, B, C, B_post) :-
   T3 = sommets(T1, T2, _),
   ( C = pioche         -> true
   ; member(C, M)       -> format("La stratégie ~w a triché durant la pioche : carte reprise ! PS : ~w, M : ~w, T3 : ~w, B : ~w, C : ~w\n", [S, PS, M, T3, B, C]),
                           assertion( false )
   ; append(T1, T2, T),
     member(C, T)       -> true
   ; true               -> format("La stratégie ~w a triché durant la pioche : carte imaginaire ! PS : ~w, M : ~w, T3 : ~w, B : ~w, C : ~w\n", [S, PS, M, T3, B, C]),
                           assertion( false )
   ),
   ( nonvar(B_post) -> true
   ; true           -> format("La stratégie ~w a perdu son cerveau durant la pioche ! PS : ~w, M : ~w, T3 : ~w, B : ~w, C : ~w\n", [S, PS, M, T3, B, C]),
                       assertion( false )
   ).

%! pli(+S: atom, +M: [carte], +T3: sommets, -P: pli, -M_post: [{ carte, 'pioche' }], -T3_post: sommets) is det.
%
% La première étape lorsqu'un joueur joue est, éventuellement, de se défausser de certaines de ces cartes, celles pouvant former une combinaison, puis d'en piocher obligatoirement une dans l'un des sommets (non vide).
%
% @arg S         Une stratégie de jeu
% @arg PS        Les plis précédents sur un tour
% @arg M         La main du joueur
% @arg T3        Les trois sommets de piles de cartes sur la table
% @arg B         L'état du cerveau (brain) du joueur
% @arg P         La pioche effectuée (ou pas)
% @arg M_post    La main du joueur après le pli, la première "carte" pouvant être 'pioche'
% @arg T3_post   Les trois sommets de piles après le pli
% @arg B_post    L'état du cerveau (brain) du joueur après avoir réfléchi
%
% @throws Precondition.   La main n'est pas vide.
%
% @throws Postcondition.   La nouvelle main ne peut pas être vide (puisqu'il faut prendre une carte dans l'un des sommets).
% @throws Postcondition.   La nouvelle main est différente de l'ancienne, sauf éventuellement si les sommets sont devenus vides.
% @throws Postcondition.   Seule la première carte de la nouvelle main peut être 'pioche'.
%
pli(_, _, [], _, _, _, _, _, _) :-
   format("Precondition 'pli(_, _, [], _, _, _, _, _, _)' violated:  empty hand\n", []),
   assertion( false ).
%
pli(S, PS, M, T3, B, pli(D, P), M_post, T3_post, B_post) :-
   assertion( (nonvar(S), nonvar(PS), nonvar(M), nonvar(T3), nonvar(B)) ),
   pli_defausse(S, PS, M, T3, B, D, M_inter, T3_inter, B_inter),
   sommets_pioche(T3_inter, D, T3_pioche),
   pli_pioche(S, PS, M_inter, T3_inter, T3_pioche, B_inter, P, M_post, T3_post, B_post),
   pli_sans_triche(S, M, P).  % L'arbitre vérifie qu'il n'y a pas eu de triche entre la défausse et la pioche.


%! pli_defausse(+S, +PS, +M, +T3, +B, -D, -M_post, -T3_post, -B_post) is det.
%
pli_defausse(S, PS, M, T3, B, D, M_post, T3_post, B_post) :-
   assertion( (nonvar(S), nonvar(PS), nonvar(M), nonvar(T3), nonvar(B)) ),
   defausse(S, PS, M, T3, B, D, B_post),    % Tout d'abord, on se défausse,
   D = defausse(K, _),
   cartes_combinaison(CS, K),               % d'une carte ou d'une combinaison de cartes
   subtract(M, CS, M_post),                 % que l'on retire de la main
   deposer_cartes_sommets(T3, D, T3_post).  % pour la mettre sur l'une des deux piles visibles.

%! sommets_pioche(+T3: sommets, +D, -T3_pioche) is det.
%
% @arg T3          Les trois sommets de cartes sur la table
% @arg B           L'état du cerveau ("brain") du joueur
% @arg T3_pioche   Les deux ou trois sommets "piochables"
%
sommets_pioche(T, 'aucune defausse', T).                                         % S'il n'y a pas y avoir de défausse, toutes les cartes sur la table sont susceptibles d'être piochées.
sommets_pioche(sommets(_, T2, N_P), defausse(_, pile_1), sommets([], T2, N_P)).  % Sinon, sont exclues les cartes qui viennent tout juste d'être déposées, sur la pile 1 ou
sommets_pioche(sommets(T1, _, N_P), defausse(_, pile_2), sommets(T1, [], N_P)).  % la pile 2.

%! pli_pioche(+S, +PS, +M, +T3, +B, -C, -M_post, -T3_post, -B_post) is det.
%
pli_pioche(_, _, M, T3, T3_pioche, B, 'aucune pioche', M, T3, B) :-     % Ensuite, on en reste là,
   assertion( (nonvar(M), nonvar(T3), nonvar(T3_pioche), nonvar(B)) ),
   sommets_vides(T3_pioche),                                            % s'il n'est pas possible de piocher quelque carte que ce soit,
   !.
pli_pioche(S, PS, M, T3, T3_pioche, B, C, M_post, T3_post, B_post) :-
   assertion( (nonvar(S), nonvar(PS)) ),
   assertion( \+ sommets_vides(T3_pioche) ),
   pioche(S, PS, M, T3_pioche, B, C, B_post),                           % la pioche devient obligatoire,
   retirer_carte_sommets(T3, C, T3_post),                               % la carte étant retirée de son sommet de pile,
   M_post = [C | M].                                                    % et ajoutée à la main finale (valant 'pioche' éventuellement).

pli_sans_triche(S, M, C) :-
   ( C = pioche   -> true
   ; member(C, M) -> format("La stratégie ~w a triché durant la pioche : carte reprise ! M : ~w, C : ~w\n", [S, M, C]),
                     assertion( false )
   ;                 true
   ).

:- begin_tests(pli).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 50, _)
                      , is_non_empty_random_subset_of_at_most(M, J, 11)
                      , subtract(J, M, JR)
                      , random_sommets(JR, T3)
                      , \+ sommets_vides(T3)
                      , strategie(S)
                      , S \= humaine
                      , cerveau_defaut(S, B)
                      )
                    , aggregate_all(count, pli(S, [], M, T3, B, _, _, _, _), 1)
                    ) ).

test(nouvelle_main_non_vide) :-
   jeu(J),
   assertion( forall( ( between(1, 50, _)
                      , is_non_empty_random_subset_of_at_most(M, J, 11)
                      , subtract(J, M, JR)
                      , random_sommets(JR, T3)
                      , \+ sommets_vides(T3)
                      , strategie(S)
                      , S \= humaine
                      , cerveau_defaut(S, B)
                      )
                    , pli(S, [], M, T3, B, _, [_ | _], _, _)
                    ) ).

test(nouvelle_main_differente_sauf_exception) :-
   jeu(J),
   assertion( forall( ( between(1, 50, _)
                      , is_non_empty_random_subset_of_at_most(M, J, 11)
                      , subtract(J, M, JR)
                      , random_sommets(JR, T3)
                      , \+ sommets_vides(T3)
                      , strategie(S)
                      , S \= humaine
                      , cerveau_defaut(S, B)
                      , pli(S, [], M, T3, B, P, M_post, _, _)
                      )
                    , ( sort(M, M_sorted)
                      , sort(M_post, M_post_sorted)
                      , ( M_sorted \= M_post_sorted
                        ; P = pli('aucune defausse', 'aucune pioche')
                        )
                      )
                    ) ).

test(si_pioche_premiere) :-
   jeu(J),
   assertion( forall( ( between(1, 50, _)
                      , is_non_empty_random_subset_of_at_most(M, J, 11)
                      , subtract(J, M, JR)
                      , random_sommets(JR, T3)
                      , \+ sommets_vides(T3)
                      , strategie(S)
                      , S \= humaine
                      , cerveau_defaut(S, B)
                      , pli(S, [], M, T3, B, _, [_ | CS], _, _)
                      )
                    , forall( member(C, CS)
                            , C \= pioche
                            )
                    ) ).

:- end_tests(pli).

%! jouer_pli(+J: atom, +PS: [pli], +M: [carte], +P3: piles, +B, -M_post: [carte], -P3_post: piles, -B_post) is det.
%
% En l'absence d'annonce, un joueur doit jouer un "pli", c'est-à-dire se défausser éventuellement d'une combinaison puis prendre une carte sur la table.
%
% @arg J         Le joueur
% @arg PS        Les quelques plis précédents du dernier tour vis-à-vis du joueur
% @arg M         La main du joueur
% @arg P3        Les trois piles sur la table
% @arg B         L'état du cerveau (brain) du joueur
% @arg P         Le pli joué
% @arg M_post    La main du joueur après le pli
% @arg P3_post   Les trois piles sur la table après le pli
% @arg B_post    L'état du cerveau (brain) du joueur après avoir joué le pli
%
% @throws Postcondition.   La main après le pli ne contient que des cartes, c'est-à-dire que la pioche a été effectuée si elle avait été demandée.
% @throws Postcondition.   Le joueur ne peut pas avoir plus d'une carte de plus en main à la fin du pli.
%                          Plus exactement, le nombre de cartes est égal au nombre de cartes initial, moins le nombre de cartes de la combinaison, si défausse il y a eu, plus une carte piochée, si les piles sur la table n'étaient pas toutes vides. 
% @throws Postcondition.   Toutes les cartes dans les piles et dans la main du joueur sont les mêmes avant et après le pli.
%                          Il n'y a eu qu'une permutation locale.
%
jouer_pli(J, PS, M, P3, B, P, M_post, P3_post, B_post) :-
   assertion( (nonvar(J), nonvar(PS), nonvar(M), nonvar(P3), nonvar(B)) ),
   joueur(J, S, _),                                       % On retrouve la stratégie associée au joueur.
   debut_tour_joueur(S, J),                               % Pour un humain, on l'informe que c'est à son tour.
   sommets_piles(T3, P3),                                 %                                           et visibles sur le haut des piles
   pli(S, PS, M, T3, B, P, M_inter, T3_post, B_post),     % pour jouer le pli avec les cartes en main                                  .
   remplacer_sommets_piles(T3_post, P3, P3_inter),        % Les nouveaux sommets des piles visibles prennent la place des anciens
   retourner_pioche(M_inter, P3_inter, M_post, P3_post),  % et on retourne la pioche si nécessaire.
   fin_tour_joueur(S, P, M_post).                         % Pour un humain, on lui résume son pli complet et on l'informe, le cas échéant, de la carte piochée.

%! debut_tour_joueur(humaine, +J) is det.
%
debut_tour_joueur(humaine, J) :-
   assertion( nonvar(J) ),
   format("\n\n***\n\nÀ votre tour ~w !\n\n", [J]).
debut_tour_joueur(S, _) :-
   S \= humaine.

%! fin_tour_joueur(humaine, +P, +M) is det.
%
fin_tour_joueur(humaine, P, M) :-
   assertion( (nonvar(P), nonvar(M)) ),
   pretty_pli(P, T_P),
   pretty_cartes(M, T_M),
   format("Vous avez joué ~w\nVotre nouvelle main est ~w\n\n***\n\nAux suivants !\n", [T_P, T_M]).
fin_tour_joueur(S, _, _) :-
   S \= humaine.

%! retourner_pioche(+M, +P3, -M_post, -P3_post) is det.
%
retourner_pioche([pioche | M], P3, [C | M], P3_post) :-  % Si le joueur a choisi de piocher, il entre dans sa nouvelle main
   assertion( (nonvar(M), nonvar(P3)) ),
   piocher(P3, C, P3_post),                              % la carte récupérée au sommet de la pile de pioche.
   !.
retourner_pioche(M, P3, M, P3).                          % Sinon, la main reste identique ainsi que l'état des trois piles.

:- begin_tests(jouer_pli).

test(det) :-
   jeu(CS),
   assertion( forall( ( between(1, 100, _)
                      , joueur(J, S, B)
                      , S \= humaine
                      , distribution(CS, [M | _], P3)
                      )
                    , aggregate_all(count, jouer_pli(J, [], M, P3, B, _, _, _, _), 1)
                    ) ).

test(pioche_effectuee) :-
   jeu(CS),
   assertion( forall( ( between(1, 100, _)
                      , joueur(J, S, B)
                      , S \= humaine
                      , distribution(CS, [M | _], P3)
                      , jouer_pli(J, [], M, P3, B, _, M_post, _, _)
                      )
                    , forall( member(C, M_post)
                            , carte(C, _, _)
                            )
                    ) ).

test(pas_plus) :-
   jeu(CS),
   assertion( forall( ( between(1, 100, _)
                      , joueur(J, S, B)
                      , S \= humaine
                      , distribution(CS, [M | _], P3)
                      , jouer_pli(J, [], M, P3, B, _, M_post, _, _)
                      )
                    , ( length(M, N_M)
                      , plus(N_M, 1, N_M_plus_1)
                      , length(M_post, N_M_post)
                      , N_M_post =< N_M_plus_1
                      )
                    ) ).

test(petite_permutation) :-
   jeu(CS),
   assertion( forall( ( between(1, 100, _)
                      , joueur(J, S, B)
                      , S \= humaine
                      , distribution(CS, [M | _], P3)
                      , jouer_pli(J, [], M, P3, B, _, M_post, P3_post, B_post)
                      )
                    , ( ramasser_cartes(P3, [etat_joueur(J, M, B)], CS_pre)
                      , sort(CS_pre, CS_sorted)
                      , ramasser_cartes(P3_post, [etat_joueur(J, M_post, B_post)], CS_post)
                      , sort(CS_post, CS_sorted)
                      )
                    ) ).

test(cerveau_final_strategie_gloutonne) :-
   jeu(CS),
   assertion( forall( ( between(1, 100, _)
                      , joueur(J, gloutonne, B)
                      , distribution(CS, [M | _], P3)
                      , jouer_pli(J, [], M, P3, B, _, _, _, B_post)
                      )
                    , B \= B_post
                    ) ).

:- end_tests(jouer_pli).

%! annonce_strategique(+S: atom, +M: [carte], +T3: sommets, +B, -A: { 'sans annonce', 'moins de neuf' }, -B_post) is det.
%
% Chaque "Intelligence artificielle" (ou joueur humain) réfléchit ici au fait d'annoncer "moins de neuf" ou pas.
%
% @arg S        Une stratégie de jeu
% @arg PS       Les plis précédents
% @arg M        Une main permettant d'annoncer "moins de neuf"
% @arg T3       Les sommets des trois piles sur la table
% @arg A        L'annonce, ou son absence
% @arg B        L'état du cerveau ("brain") du joueur
% @arg B_post   L'état du cerveau ("brain") du joueur après avoir réfléchi
%
% @throws Precondition.   Les cartes en main totalisent strictement moins de neuf points.
%
% @throws Postcondition.   Le prédicat réussit si la stratégie décide d'annoncer "moins de neuf".
%
annonce_strategique(_, _, M, _, _, _, _) :-
   assertion( nonvar(M) ),
   points_cartes(M, P),
   P >= 9,
   format("Precondition 'annonce_strategique(_, _, ~w, _, _, _, _)' violated:  too many points in hand\n", [M]),
   assertion( false ).
%
% stratégie primitive
%
annonce_strategique(primitive, _, _, _, B, 'moins de neuf', B) :-  % La stratégie primitive s'arrête dès qu'elle le peut.
   assertion( nonvar(B) ).
%
% stratégie aléatoire
%
annonce_strategique(aleatoire, _, _, _, B, 'moins de neuf', B) :-  % La stratégie aléatoire se lance,
   assertion( nonvar(B) ),
   random_between(1, 4, Pr),                                       % arbitrairement,
   between(1, 3, Pr),                                              % trois fois sur quatre.
   !.
annonce_strategique(aleatoire, _, _, _, B, 'sans annonce', B).
%
% stratégie gloutonne
%
annonce_strategique(gloutonne, _, M, _, cerveau_glouton(B1, B2, B3), 'moins de neuf', cerveau_glouton(B1, B2, B3_plus_1)) :-  % La stratégie gloutonne s'arrête,
   assertion( nonvar(M) ),
   assertion( ( nonvar(B1), nonvar(B2), nonvar(B3)) ),
   points_cartes(M, P),                                            % en fonction des points de la main,
   member((P, Pr), [ (1, 100)                                      % avec une probabilité d'autant plus faible que les points sont élevés,
                   , (2,  77)                                      % ici en suivant une loi exponentielle : (1 - (P - 1) / 8)^2.
                   , (3,  56)
                   , (4,  39)
                   , (5,  25)
                   , (6,  14)
                   , (7,   6)
                   , (8,   2)
                   ]),
   random_between(1, 100, A),
   A =< Pr,
   !,
   plus(B3, 1, B3_plus_1).
annonce_strategique(gloutonne, _, _, _, cerveau_glouton(B1, B2, B3), 'sans annonce', cerveau_glouton(B1, B2, B3_plus_1)) :-
   plus(B3, 1, B3_plus_1).
%
% stratégie humaine
%
annonce_strategique(humaine, PS, M, T3, B, A, B) :-  % Comme toujours, pour la stratégie humaine, il suffit de demander au joueur humain.
   assertion( (nonvar(PS), nonvar(M), nonvar(T3), nonvar(B)) ),
   presenter_plis_precedents(PS),
   presenter_main(M),
   presenter_sommets(T3),
   points_cartes(M, P),
   format("Vous totalisez ~w points\n", [P]),
   ( demander_pose -> A = 'moins de neuf'
   ;                  A = 'sans annonce'
   ).

%! demander_pose is semidet.
%
demander_pose :-
   format("Voulez-vous annoncer \"moins de neuf\" ? (oui ou non)\n", []),
   current_input(Stdin),
   read_line_to_string(Stdin, S),
   string_to_atom(S, A),
   ( A = oui -> true
   ; A = non -> fail
   ;            demander_pose
   ).

%! annonce(+S: atom, +M: [carte], +T3: sommets, +B: brain, -A: { 'sans annonce', 'moins de neuf' }, -B_post: brain) is det.
%
% Vérification de la possibilité, puis l'envie, d'annoncer la fin de la manche par "moins de neuf".
%
% @arg S        Une stratégie de jeu
% @arg PS       Les plis précédents
% @arg M        La main du joueur
% @arg T3       Les sommets des trois piles sur la table
% @arg B        L'état du cerveau (brain) du joueur
% @arg A        L'annonce, ou son absence
% @arg B_post   L'état du cerveau (brain) du joueur après avoir réfléchi
%
% @throws Postcondition.   Le prédicat réussit si la stratégie a pu et a décidé d'annoncer "moins de neuf".
%
annonce(_, _, M, _, B, 'sans annonce', B) :-  % Il ne peut pas y avoir d'annonce
   assertion( (nonvar(M), nonvar(B)) ),
   points_cartes(M, P),               % si le nombre de points dans la main
   P >= 9.                            % est supérieur ou égal à neuf.
annonce(S, PS, M, T3, B, A, B_post) :-
   assertion( (nonvar(S), nonvar(PS), nonvar(T3)) ),
   points_cartes(M, P),                              % En revanche, si le nombre de points dans la main
   P < 9,                                            % est strictement inférieur à neuf,
   annonce_strategique(S, PS, M, T3, B, A, B_post),  % alors le joueur peut envisager de lancer l'annonce.
   !,                                                % (Ici, on assure qu'il n'y aura pas de point de retour dû à une décision incomplète de l'agent.)
   annonce_sans_triche(S, PS, M, T3, B, A, B_post).  % Puis, l'arbitre vérifie qu'elle ne triche pas... et pourra continuer à réfléchir !

annonce_sans_triche(S, PS, M, T3, B, A, B_post) :-
   ( A = 'sans annonce'  -> true
   ; A = 'moins de neuf' -> true
   ; true                -> format("La stratégie ~w a triché durant l'annonce ! PS : ~w, M : ~w, T3 : ~w, B : ~w, A : ~w\n", [S, PS, M, T3, B, A]),
                            assertion( false )
   ),
   ( nonvar(B_post) -> true
   ; true           -> format("La stratégie ~w a perdu son cerveau durant la défausse ! PS : ~w, M : ~w, T3 : ~w, B : ~w, A : ~w\n", [S, PS, M, T3, B, A]),
                       assertion( false )
   ).

:- begin_tests(annonce).

test(det) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , joueur(_, S, B)
                      , S \= humaine
                      , distribution(J, [M | _], P3)
                      , sommets_piles(T3, P3)
                      )
                    , aggregate_all(count, annonce(S, [], M, T3, B, _, _), 1)
                    ) ).

test(annonce) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , joueur(_, S, B)
                      , S \= humaine
                      , distribution(J, [M | _], P3)
                      , sommets_piles(T3, P3)
                      , annonce(S, [], M, T3, B, A, _)
                      )
                    , member(A, ['sans annonce', 'moins de neuf'])
                    ) ).

test(cerveau_final_strategie_gloutonne) :-
   jeu(J),
   assertion( forall( ( between(1, 100, _)
                      , joueur(J, gloutonne, B)
                      , distribution(J, [M | _], P3)
                      , sommets_piles(T3, P3)
                      , annonce(gloutonne, [], M, T3, B, _, B_post)
                      )
                    , B \= B_post
                    ) ).

:- end_tests(annonce).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% MANCHE
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% La manche est la partie centrale d'un jeu.
% Une partie ce jouera en un certain nombre de manches.
% Chaque manche est constituée d'une séquence de "plis".
% C'est à l'issue d'une manche, que les scores des joueurs sont calculés.
%
% Une manche débute par la distribution des cartes, par mélange "manuel".
% s'en suit une rotation des joueurs qui enchaînent des plis.
% Lorsqu'un joueur annonce "moins de neuf", on finit la manche avec un dernier tour jusqu'à l'annonceur exclu.
% Les cartes en main sont alors dévoilées, les points comptés et les scores établis.
% Les cartes sont alors remises en paquet en empilant les cartes sur la tables et celles dans les mains des joueurs.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%








%! gagnants_manche(+A: atom, +JPS: [(atom, nat)], -GS: [atom]) is det.
%
% Si l'annonceur a bien joué, il est l'unique gagnant de la manche.
% Dans le cas contraire, parmi tous ceux qui n'ont pas dépassé son score, sont déclarés gagnants ex æquo tous ceux qui ont obtenu un score minimal.
%
% @throws Precondition.   L'annonceur est l'un des joueurs.
% @throws Precondition.   Les joueurs sont distincts.
%
% @throws Postcondition.   L'ensemble des gagnants n'est pas vide.
% @throws Postcondition.   S'il y a plusieurs gagnants, il ne peut pas y avoir l'annonceur parmi eux.
%
gagnants_manche(A, JPS, _) :-
   assertion( (nonvar(A), nonvar(JPS)) ),
   \+ member((A, _), JPS),
   format("Precondition 'gagnants_manche(~w, ~w, _)' violated:  missing player\n", [A, JPS]),
   assertion( false ).
gagnants_manche(_, JPS, _) :-
   maplist([J, (J, _)]>>true, JS, JPS),
   \+ all_different(JS),
   format("Precondition 'gagnants_manche(_, ~w, _)' violated:  duplicated player\n", [JPS]),
   assertion( false ).
%
gagnants_manche(A, JPS, [A]) :-      % L'annonceur est l'unique gagnant de la manche
   member((A, S_A), JPS),
   forall( ( member((J, S_J), JPS)   % si, parmi tous les
           , J \= A                  % _autres_ joueurs,
           )
         , S_A < S_J                 % aucun n'a strictement moins de points en main que lui.
         ).
gagnants_manche(A, JPS, GS) :-       % Sinon, sont déclarés gagnants ex æquo
   member((A, S_A), JPS),
   exclude([(A, _)]>>true, JPS, JPS_moins_A),        % Pour l'ensemble des _autres_ joueurs,
   maplist([(_, S_J), S_J]>>true, JPS_moins_A, SS),  % on récupère leurs scores
   min_list(SS, S_min),                              % afin de déterminer le meilleur score des non perdants.
   S_min =< S_A,                                     % Dès lors que le score minimal est inférieur ou égal à celui de l'annonceur
   include([(_, S_min)]>>true, JPS_moins_A, GSS),    % tous ceux qui totalisent ce score-là,
   maplist([(J, _), J]>>true, GSS, GS).              % sont les véritables gagnants de la manche.

:- begin_tests(gagnants_manche).

test(det) :-
   jeu(CS),
   joueurs(JS_full),
   maplist([J, joueur(J, _, _)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , distribution(CS, MS, _)
                      , maplist([J, M, (J, P)]>>points_cartes(M, P), JS, MS, JPS)
                      , random_member(A, JS)
                      )
                    , aggregate_all(count, gagnants_manche(A, JPS, _), 1)
                    ) ).

test(non_vide) :-
   jeu(CS),
   joueurs(JS_full),
   maplist([J, joueur(J, _, _)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , distribution(CS, MS, _)
                      , maplist([J, M, (J, P)]>>points_cartes(M, P), JS, MS, JPS)
                      , random_member(A, JS)
                      )
                    , gagnants_manche(A, JPS, [_ | _])
                    ) ).

test(gagnants_non_annonceur_si_ex_aequo) :-
   jeu(CS),
   joueurs(JS_full),
   maplist([J, joueur(J, _, _)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , distribution(CS, MS, _)
                      , maplist([J, M, (J, P)]>>points_cartes(M, P), JS, MS, JPS)
                      , random_member(A, JS)
                      , gagnants_manche(A, JPS, GS)
                      )
                    , ( member(A, GS) -> GS = [A]
                      ;                  true
                      )
                    ) ).

:- end_tests(gagnants_manche).

%! perdants_manche(+A: atom, +JPS: [(atom, nat)], -P: [atom]) is det.
%
% Si l'annonceur n'a pas bien joué, il est l'unique perdant de la manche.
% Dans le cas contraire, parmi tous les autres joueurs, on choisit au hasard l'un de ceux qui a un score maximal.
% Dans le cas contraire, parmi tous ceux qui n'ont pas réussi à faire au moins aussi bien que son score, sont déclarés perdants ex æquo tous ceux qui ont obtenu un score maximal.
%
% @throws Precondition.   L'annonceur est l'un des joueurs.
% @throws Precondition.   Les joueurs sont distincts.
%
% @throws Postcondition.   L'ensemble des perdants n'est pas vide.
% @throws Postcondition.   Pour que l'annonceur soit perdant, il suffit qu'au moins un des autres joueurs ait marqué au plus autant de points que lui.
%
perdants_manche(A, JPS, _) :-
   assertion( (nonvar(A), nonvar(JPS)) ),
   \+ member((A, _), JPS),
   format("Precondition 'perdants_manche(~w, ~w, _)' violated:  missing player\n", [A, JPS]),
   assertion( false ).
perdants_manche(_, JPS, _) :-
   maplist([J, (J, _)]>>true, JS, JPS),
   \+ all_different(JS),
   format("Precondition 'perdants_manche(_, ~w, _)' violated:  duplicated player\n", [JPS]),
   assertion( false ).
%
perdants_manche(A, JPS, [A]) :-                      % L'annonceur est l'unique perdant de la manche...
   \+ gagnants_manche(A, JPS, [A]).                  % s'il n'est pas le gagnant !
perdants_manche(A, JPS, PS) :-
   gagnants_manche(A, JPS, [A]),                     % Sinon, on sait que tous les autres joueurs ont marqués strictement plus que points que lui.
   exclude([(A, _)]>>true, JPS, JPS_moins_A),        % Pour l'ensemble des _autres_ joueurs,
   maplist([(_, S_J), S_J]>>true, JPS_moins_A, SS),  % on récupère leurs scores
   max_list(SS, S_max),                              % afin de déterminer le plus mauvais score des non gagnants.
   include([(_, S_max)]>>true, JPS_moins_A, PSS),    % Tous ceux qui totalisent ce score-là,
   maplist([(J, _), J]>>true, PSS, PS).              % sont les véritables perdants de la manche. (On y choisira au hasard un perdant qui aura l'avantage d'entamer la manche suivante.)

:- begin_tests(perdants_manche).

test(det) :-
   jeu(CS),
   joueurs(JS_full),
   maplist([J, joueur(J, _, _)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , distribution(CS, MS, _)
                      , maplist([J, M, (J, P)]>>points_cartes(M, P), JS, MS, JPS)
                      , random_member(A, JS)
                      )
                    , aggregate_all(count, perdants_manche(A, JPS, _), 1)
                    ) ).

test(non_vide) :-
   jeu(CS),
   joueurs(JS_full),
   maplist([J, joueur(J, _, _)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , distribution(CS, MS, _)
                      , maplist([J, M, (J, P)]>>points_cartes(M, P), JS, MS, JPS)
                      , random_member(A, JS)
                      )
                    , perdants_manche(A, JPS, [_ | _])
                    ) ).

test(annonceur_non_perdant) :-
   jeu(CS),
   joueurs(JS_full),
   maplist([J, joueur(J, _, _)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , distribution(CS, MS, _)
                      , maplist([J, M, (J, P)]>>points_cartes(M, P), JS, MS, JPS)
                      , random_member(A, JS)
                      , perdants_manche(A, JPS, PS)
                      )
                    , ( PS = [A] -> ( member((A, S_A), JPS)
                                    , member((J, S_J), JPS)
                                    , J \= A
                                    , S_J =< S_A
                                    )
                      ;             ( member((A, S_A), JPS)
                                    , forall( ( member((J, S_J), JPS)
                                              , J \= A
                                              )
                                            , S_J > S_A
                                            )
                                    )
                      )
                    ) ).

:- end_tests(perdants_manche).

%! scores_manche(+A: joueur, +JMS: [(joueur, [carte])], -JSS: [(joueur, {0, 1, 2})]) is det.
%
% @arg A     Le joueur ayant lancé l'annonce
% @arg JMS   Liste des joueurs avec leur main en fin de manche
% @arg JSS   Liste des joueurs avec leur score de la manche
%
% @throws Precondition.   L'annonceur est l'un des joueurs.
% @throws Precondition.   Les joueurs sont distincts.
%
% @throws Postcondition.   Les joueurs sont les mêmes.
% @throws Postcondition.   Il y a au moins un gagnant.
% @throws Postcondition.   Il y a au moins un perdant.
%
scores_manche(A, JMS, _) :-
   assertion( (nonvar(A), nonvar(JMS)) ),
   \+ member((A, _), JMS),
   format("Precondition 'scores_manche(~w, ~w, _)' violated:  missing player\n", [A, JMS]),
   assertion( false ).
scores_manche(_, JMS, _) :-
   maplist([J, (J, _)]>>true, JS, JMS),
   \+ all_different(JS),
   format("Precondition 'scores_manche(_, ~w, _)' violated:  duplicated player\n", [JMS]),
   assertion( false ).
%
scores_manche(A, JMS, JSS) :-
   maplist([(J, M), (J, P)]>>points_cartes(M, P), JMS, JPS),
   gagnants_manche(A, JPS, GS),
   perdants_manche(A, JPS, PS),
   maplist([(J, _), (J, S)]>>score_joueur(J, GS, PS, S), JMS, JSS).
   /* La version suivante génère des avertissements lors d'un rechargement du programme (?!).
   maplist([(J, _), (J, S)]>>( member(J, GS) -> S = 2
                             ; member(J, PS) -> S = 0
                             ;                  S = 1
                             ), JMS, JSS).
   */
%
score_joueur(J, GS, _, 2) :- % TODO
   member(J, GS).
score_joueur(J, _, PS, 0) :-
   member(J, PS).
score_joueur(J, GS, PS, 1) :- % TODO 1
   \+ member(J, GS),
   \+ member(J, PS).
   

:- begin_tests(scores_manche).

test(det) :-
   jeu(CS),
   joueurs(JS_full),
   maplist([J, joueur(J, _, _)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , distribution(CS, MSS, _)
                      , maplist([J, MS, (J, MS)]>>true, JS, MSS, JMS)
                      , random_member(A, JS)
                      )
                    , aggregate_all(count, scores_manche(A, JMS, _), 1)
                    ) ).

test(memes_joueurs) :-
   jeu(CS),
   joueurs(JS_full),
   maplist([J, joueur(J, _, _)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , distribution(CS, MSS, _)
                      , maplist([J, MS, (J, MS)]>>true, JS, MSS, JMS)
                      , random_member(A, JS)
                      , scores_manche(A, JMS, JSS)
                      )
                    , forall( member((J, _), JMS)
                            , member((J, _), JSS)
                            )
                    ) ).

test(gagnants_existent) :-
   jeu(CS),
   joueurs(JS_full),
   maplist([J, joueur(J, _, _)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , distribution(CS, MSS, _)
                      , maplist([J, MS, (J, MS)]>>true, JS, MSS, JMS)
                      , random_member(A, JS)
                      , scores_manche(A, JMS, JSS)
                      )
                    , ( include([(_, 2)]>>true, JSS, GS)
                      , length(GS, N_G)
                      , N_G >= 1
                      )
                    ) ).

test(perdants_existent) :-
   jeu(CS),
   joueurs(JS_full),
   maplist([J, joueur(J, _, _)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , distribution(CS, MSS, _)
                      , maplist([J, MS, (J, MS)]>>true, JS, MSS, JMS)
                      , random_member(A, JS)
                      , scores_manche(A, JMS, JSS)
                      )
                    , ( include([(_, 0)]>>true, JSS, PS)
                      , length(PS, N_P)
                      , N_P >= 1
                      )
                    ) ).

test(valeurs_scores) :-
   jeu(CS),
   joueurs(JS_full),
   maplist([J, joueur(J, _, _)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , distribution(CS, MSS, _)
                      , maplist([J, MS, (J, MS)]>>true, JS, MSS, JMS)
                      , random_member(A, JS)
                      , scores_manche(A, JMS, JSS)
                      )
                    , forall( member((J, S), JSS)
                            , member(S, [0, 1, 2])
                            )
                    ) ).

:- end_tests(scores_manche).

%! preparer_manche(+J:  [carte], +JS: [joueur(atom, brain)], +D: joueur, -P3: piles, -JMBS: [etat_joueur(atom, [carte], brain) })]) is det.
%
% @arg J       Le jeu de cartes
% @arg JS      Les joueurs, ordonnés
% @arg D       Le donneur, joueur qui distribue les cartes en début de partie et qui entame
% @arg P3      Les cartes sur les trois piles de la table
% @arg JMBS    Les joueurs avec leurs informations associées : main et cerveau
%
% @throws Precondition.   Le donneur est l'un des joueurs.
%
% @throws Postcondition.   Le donneur est le premier dans la liste des joueurs prêts à jouer.
% @throws Postcondition.   Les joueurs sont les mêmes avant et après à une rotation près.
% @throws Postcondition.   Les cartes dans les mains des joueurs et sur la table constituent une permutation du jeu de carte.
%
preparer_manche(_, JS, D, _, _) :-
   assertion( (nonvar(JS), nonvar(D)) ),
   \+ member(joueur(D, _), JS),
   format("Precondition 'preparer_manche(_, ~w, ~w, _, _)' violated:  missing first player\n", [JS, D]),
   assertion( false ).
%
preparer_manche(J, JS, D, P3, JMBS) :-
   assertion( nonvar(J) ),
   distribution(J, MS, P3),                                                     % Au début d'une manche, des cartes sont distribuées (par le donneur) aux joueurs et celles qui restent mises sur les trois piles sur la table.
   maplist([joueur(X, Y), M, etat_joueur(X, M, Y)]>>true, JS, MS, JMBS_inter),  % L'état de chaque joueur est initialisé avec son identité, sa main et le dernier pli fait, vide en l'occurrence.
   append(JS1, [etat_joueur(D, M_D, B_D) | JS2], JMBS_inter),                   % Puis, l'état du donneur
   append([etat_joueur(D, M_D, B_D) | JS2], JS1, JMBS).                         % est positionné en début de liste puisqu'il entame la manche.

:- begin_tests(preparer_manche).

test(det) :-
   jeu(J),
   joueurs(JS_full),
   maplist([joueur(A, B), joueur(A, _, B)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , random_member(joueur(D, _), JS)
                      )
                    , aggregate_all(count, preparer_manche(J, JS, D, _, _), 1)
                    ) ).

test(donneur_premier) :-
   jeu(J),
   joueurs(JS_full),
   maplist([joueur(A, B), joueur(A, _, B)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , random_member(joueur(D, _), JS)
                      , preparer_manche(J, JS, D, _, JS_post)
                      )
                    , JS_post = [etat_joueur(D, _, _) | _]
                    ) ).

test(rotation_joueurs) :-
   jeu(J),
   joueurs(JS_full),
   maplist([joueur(A, B), joueur(A, _, B)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , random_member(joueur(D, _), JS)
                      , preparer_manche(J, JS, D, _, JS_post)
                      )
                    , ( maplist([joueur(A, _), A]>>true, JS, JS_nom)
                      , maplist([etat_joueur(A, _, _), A]>>true, JS_post, JS_nom_rot)
                      , rotate(JS_nom, JS_nom_rot)
                      )
                    ) ).

:- end_tests(preparer_manche).

%! ramasser_cartes(+P3: piles, +JMBS: [etat_joueur(atom, {0, 1, 2}, brain)], -J: [carte]) is det.
%
% @arg P3     Les cartes sur les trois piles de la table
% @arg JMBS   Les états des joueurs (a priori en fin de manche)
% @arg J      Le jeu de cartes
%
% @throws Postcondition.   Si l'on distribue puis ramasse immédiatement des cartes, on obtient une permutation des cartes d'origine.
%
ramasser_cartes(piles(P1, P2, P), JMBS, J) :-
   assertion( (nonvar(P1), nonvar(P2), nonvar(P), nonvar(JMBS)) ),
   append(P1, CS1),                                     % On ramasse la première pile
   append(P2, CS2),                                     % ainsi que la seconde
   findall(M, member(etat_joueur(_, M, _), JMBS), MS),  % On ramasse alors les mains restantes des joueurs
   % maplist([M, etat_joueur(_, M, _)]>>true, MS, JMBS),
   append([CS1, CS2, P | MS], J).                       % et finalement, on empile tout cela pour reconstituer le jeu de cartes.

:- begin_tests(ramasser_cartes).

test(det) :-
   jeu(J),
   joueurs(JS_full),
   maplist([joueur(A, B), joueur(A, _, B)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , random_member(joueur(D, _), JS)
                      , preparer_manche(J, JS, D, P3, JMBS)
                      )
                    , aggregate_all(count, ramasser_cartes(P3, JMBS, _), 1)
                    ) ).

test(permutation) :-
   jeu(J),
   joueurs(JS_full),
   maplist([joueur(A, B), joueur(A, _, B)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , random_member(joueur(D, _), JS)
                      , preparer_manche(J, JS, D, P3, JMBS)
                      , ramasser_cartes(P3, JMBS, J_post)
                      )
                    , permutation(J, J_post)
                    ) ).

:- end_tests(ramasser_cartes).

%! nombre_pli_manche_maximal(?P_max: nat) is det.
%
% Chaque manche ne permet qu'un certain nombre maximal de plis par joueur.
%
% @arg P_max   Le nombre maximal de plis possible par joueur et par manche
%
% @throws Postcondition.   Ce nombre est un entier naturel strictement positif.
%
nombre_pli_manche_maximal(30).

:- begin_tests(nombre_pli_manche_maximal).

test(det) :-
   assertion( aggregate_all(count, nombre_pli_manche_maximal(_), 1) ).

test(strictement_positif) :-
   nombre_pli_manche_maximal(P_max),
   assertion( P_max > 0 ).

:- end_tests(nombre_pli_manche_maximal).


%! manche_nulle(+PS: [pli], +T3: piles, +JMBS:  [etat_joueur(atom, [carte], brain) })], -P3_post: piles, -JMBS_post: [(joueur, [carte], brain)]) is det.
%
% Pour éviter des manches infinies, une règle de terminaison de manche est ajoutée.
% Elle obéit à deux conditions.
% Tout d'abord, les joueurs ont droit à un 'nombre_pli_manche_maximal/1'.
% Ensuite, la manche se termine en l'état également si les joueurs ont tous "passé" à tour de rôle, sous la condition que les piles soient toutes vides, ce qui laisse un doute raisonnable sur la bonne volonté des joueurs.
%
% @arg PS          Les plis faits depuis le début de la manche
% @arg P3          Les trois piles sur la table au début de la manche
% @arg JMBS        L'état des joueurs au début de la manche
% @arg PS_post     Les plis avec, en tête, le nouveau pli
% @arg P3_post     Les trois piles sur la table à la fin de la manche
% @arg JMBS_post   L'état des joueurs à la fin de la manche
%
manche_nulle(PS, _, JMBS) :-          % Une manche se termine en l'état
   assertion( (nonvar(PS), nonvar(JMBS)) ),
   length(JMBS, N),                   % si les joueurs
   length(PS, N_PS),                  % ont joué un nombre de plis
   nombre_pli_manche_maximal(P_max),  
   multiply(N, P_max, N_max),
   N_PS >= N_max,                     % supérieur ou égal au maximum fixé.
   !. 
manche_nulle(PS, T3, JMBS) :-         % En outre, une manche se finit en l'état également
   assertion( nonvar(T3) ),
   sommets_vides(T3),                 % si les piles sont vides
   length(JMBS, N),                   % et si l'ensemble des joueurs,
   length(PS, N_PS),                  % sur tous leurs derniers plis,
   N_PS >= N,                         % lorsqu'au moins un tour complet a eu lieu,
   take(N, PS, PS_tour),
   forall( member(P, PS_tour)
         , P = pli('aucune defausse', 'aucune pioche')  % n'ont rien pu jouer.
         ).

%! debut_manche(+PS: [pli], +P3: piles, +JMBS:  [etat_joueur(atom, [carte], brain) })], -P3_post: piles, -JMBS_post: [(joueur, [carte], brain)]) is det.
%
% Le début d'un manche est constituée d'une séquence de plis faits tour à tour par les joueurs dans l'ordre où ils ont été placés.
% Ce début se termine avec l'annonce "moins de neuf" faite par un joueur.
%
% @arg PS          Les plis faits depuis le début de la manche
% @arg P3          Les trois piles sur la table au début de la manche
% @arg JMBS        L'état des joueurs au début de la manche
% @arg PS_post     Les plis avec, en tête, le nouveau pli
% @arg P3_post     Les trois piles sur la table à la fin de la manche
% @arg JMBS_post   L'état des joueurs à la fin de la manche
%
debut_manche(PS, P3, JMBS, PS, P3, JMBS) :-
   assertion( (nonvar(PS), nonvar(P3), nonvar(JMBS)) ),
   sommets_piles(T3, P3),
   manche_nulle(PS, T3, JMBS),
   !. % Sinon,
debut_manche(PS, P3, JMBS, PS_post, P3_post, JMBS_post) :-
   preparer_pli(PS, P3, JMBS, J, M, B, S, PS_tour, M, T3),
   annonce(S, PS_tour, M, T3, B, A, B_start),
   ( A = 'moins de neuf' -> fin_debut_manche(PS, P3, JMBS, B_start, PS_post, P3_post, JMBS_post)
   ; A = 'sans annonce'  -> ( jouer_pli(J, PS_tour, M, P3, B_start, P, M_inter, P3_inter, B_inter)
                            , JMBS = [etat_joueur(J, M, B) | JMBS_next]
                            , append(JMBS_next, [etat_joueur(J, M_inter, B_inter)], JMBS_rot)
                            , debut_manche([P | PS], P3_inter, JMBS_rot, PS_post, P3_post, JMBS_post)
                            )
   ).

%! preparer_pli(+PS, +P3, +JMBS, -J, -M, -B, -S, -PS_tour, -M, -T3) is det.
%
preparer_pli(PS, P3, [etat_joueur(J, M, B) | JMBS_next], J, M, B, S, PS_tour, M, T3) :-
   assertion( (nonvar(PS), nonvar(P3), nonvar(J), nonvar(M), nonvar(B), nonvar(JMBS_next)) ),
   joueur(J, S, _),                 % Pour jouer le pli suivant, on récupère le joueur à qui c'est le tour ainsi que sa stratégie de jeu,
   length(JMBS_next, N_J),          % les derniers plis, sur un tour,
   take_at_most(N_J, PS, PS_tour),  % des autres joueurs,
   sommets_piles(T3, P3).           % et finalement les sommets visibles des piles sur la table.

%! fin_debut_manche(+PS, +P3, +JMBS, +B_start, -PS_post, -P3_post, -JMBS_post) is det.
%
fin_debut_manche(PS, P3, [etat_joueur(J, M, _) | JMBS_next], B_start, ['moins de neuf' | PS], P3, JMBS_post) :-  % Le joueur vient d'annoncer "moins de neuf" comme (son tout) dernier "pli", les piles sur la table restent inchangés,
   assertion( (nonvar(PS), nonvar(P3), nonvar(J), nonvar(M), nonvar(JMBS_next), nonvar(B_start)) ),
   append(JMBS_next, [etat_joueur(J, M, B_start)], JMBS_post).                                                   % et l'on passe au joueur suivant pour le tour final.

:- begin_tests(debut_manche).

test(det) :-
   jeu(J),
   joueurs(JS_full),
   maplist([joueur(A, B), joueur(A, _, B)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 100, _)
                      , random_member(joueur(D, _), JS)
                      , preparer_manche(J, JS, D, P3, JMBS)
                      )
                    , aggregate_all(count, debut_manche([], P3, JMBS, _, _, _), 1)
                    ) ).

:- end_tests(debut_manche).

%! finir_manche(+PS: [pli], +P3: piles, +JMBS:  [etat_joueur(atom, [carte], brain) })], -P3_post: piles, -JMBS_post: [(joueur, [carte], brain)]) is det.
%
% La fin d'une manche est constitué d'un pli joué par chacun des joueur après que l'annonce "moins de neuf" ait été faite.
% Cela exclut l'annonceur.
%
% @arg PS          Les plis faits depuis le début de la manche
% @arg P3          Les trois piles sur la table au début de la manche
% @arg JMBS        L'état des joueurs au début de la manche
% @arg PS_post     Les plis avec, en tête, le nouveau pli
% @arg P3_post     Les trois piles sur la table à la fin de la manche
% @arg JMBS_post   L'état des joueurs à la fin de la manche
%
% @todo   La fin de manche n'est pas encore programmée...
%
finir_manche(PS, P3, JMBS, PS, P3, JMBS) :-
   assertion( (nonvar(PS), nonvar(P3), nonvar(JMBS)) ).

%! jouer_manche(+J:  [carte], +JS: [joueur(atom, brain)], +D: joueur, -P3: piles, -J_post: [carte], -JSBS_post: [etat_joueur(atom, {0, 1, 2}, brain)]) is det.
%
% @arg J           Le jeu de cartes
% @arg JS          Les joueurs, ordonnés
% @arg D           Le donneur, joueur qui distribue les cartes en début de partie et qui entame
% @arg J_post      Le jeu de cartes, ramassé en vue de la prochaine manche
% @arg JSBS_post   Les joueurs avec leurs informations associées
% @arg P           Le perdant de la manche (et donneur de la suivante, s'il y en a une)
%
% @throws Precondition.   Le donneur est l'un des joueurs.
%
% @throws Postcondition.   Le jeu de carte à l'issue de la manche est une permutation du jeu de carte en début de manche.
% @throws Postcondition.   Les joueurs en début et fin de manche sont les mêmes.
% @throws Postcondition.   Le perdant est l'un des joueurs.
%
jouer_manche(_, JS, D, _, _, _) :-
   assertion( (nonvar(JS), nonvar(D)) ),
   \+ member(joueur(D, _), JS),
   format("Precondition 'jouer_manche(_, ~w, ~w, _, _, _)' violated:  missing first player\n", [JS, D]),
   assertion( false ).
%
jouer_manche(J, JS, D, J_post, JSBS_post, P) :-
   assertion( (nonvar(J), var(J_post), var(JSBS_post), var(P)) ),
   preparer_manche(J, JS, D, P3, JMBS),                                            % Après l'installation de la manche,
   debut_manche([], P3, JMBS, PS_inter, P3_inter, JMBS_inter),                     % elle est jouée 
   continuer_manche(JS, D, PS_inter, P3_inter, JMBS_inter, J_post, JSBS_post, P).  % jusqu'à l'annonce de "moins de neuf" ou d'une manche nulle

%! continuer_manche(+JS, +D, +PS, +P3, +JMBS, -J_post, -JSBS_post, -D) is det.
%
continuer_manche(JS, _, PS, P3, JMBS, J_post, JSBS_post, P) :-
   assertion( (nonvar(JS), nonvar(PS), nonvar(P3), nonvar(JMBS)) ),
   member('moins de neuf', PS),                                    % Si l'annonce a eu lieu 
   finir_manche(PS, P3, JMBS, _, P3_post, JMBS_post),              % vient le tour final
   maplist([(X, Y), etat_joueur(X, Y, _)]>>true, JMS, JMBS),
   [etat_joueur(A, _, _) | _] = JMBS,                              % qui a été lancé par l'annonceur
   scores_manche(A, JMS, JSS),                                     % et à l'issue duquel les scores des joueurs sont calculés.
   findall( etat_joueur(Jr, Sc, Br)
          , ( member(etat_joueur(Jr, _, Br), JMBS)
            , member((Jr, Sc), JSS)
            )
          , JSBS_post
          ),
   grand_perdant(JSBS_post, P),                                    % Le perdant est choisi pour être le prochain donneur
   ramasser_cartes(P3_post, JMBS_post, J_post).                    % et on finit en ramassant les cartes.
continuer_manche(JS, D, PS, P3, JMBS, J_post, JSBS_post, D) :-     % En cas de manche nulle, le même donneur recommence,
   assertion( nonvar(D) ),
   \+ member('moins de neuf', PS),
   scores_nuls(JS, JSBS_post),                                     % Dans le cas d'une manche nulle, tous les joueurs sont déclarés perdants (pour cause d'anti-jeu)
   ramasser_cartes(P3, JMBS, J_post).                              % après avoir ramassé les cartes.

%! scores_nuls(+JBS, -JSBS) is det.
%
scores_nuls(JBS, JSBS) :-
   assertion( nonvar(JBS) ),
   findall(etat_joueur(J, 0, B), member(joueur(J, B), JBS), JSBS).
   % maplist([joueur(J, B), etat_joueur(J, 0, B)]>>true, JBS, JSBS).

%! grand_perdant(+JSBS, -P) is det.
%
grand_perdant(JSBS, P) :-
   assertion( nonvar(JSBS) ),
   findall(JSB, (member(JSB, JSBS), JSB = etat_joueur(_, 0, _)), PS),  % Parmi les perdants de la manche,
   % include([etat_joueur(_, 0, _)]>>true, JSBS, PS),
   random_member(etat_joueur(P, _, _), PS).                            % on choisit aléatoirement l'un d'entre eux, s'ils sont plusieurs, comme le grand perdant (mais celui qui aura l'avantage de l'entame à la manche suivante),

:- begin_tests(jouer_manche).

test(det) :-
   jeu(J),
   joueurs(JS_full),
   maplist([joueur(A, B), joueur(A, _, B)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 5, _)
                      , random_member(joueur(D, _), JS)
                      )
                    , aggregate_all(count, jouer_manche(J, JS, D, _, _, _), 1)
                    ) ).

test(permutation) :-
   jeu(J),
   joueurs(JS_full),
   maplist([joueur(A, B), joueur(A, _, B)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 5, _)
                      , random_member(joueur(D, _), JS)
                      , jouer_manche(J, JS, D, J_post, _, _)
                      )
                    , permutation(J, J_post)
                    ) ).

test(memes_joueurs) :-
   jeu(J),
   joueurs(JS_full),
   maplist([joueur(A, B), joueur(A, _, B)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 5, _)
                      , random_member(joueur(D, _), JS)
                      , jouer_manche(J, JS, D, _, JSBS_post, _)
                      )
                    , ( forall( member(joueur(K, _), JS)
                              , member(etat_joueur(K, _, _), JSBS_post)
                              )
                      , forall( member(etat_joueur(L, _, _), JSBS_post)
                              , member(joueur(L, _), JS)
                              )
                      )
                    ) ).

test(scores_0_1_2) :-
   jeu(J),
   joueurs(JS_full),
   maplist([joueur(A, B), joueur(A, _, B)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 5, _)
                      , random_member(joueur(D, _), JS)
                      , jouer_manche(J, JS, D, _, JSBS_post, _)
                      )
                    , forall( member(etat_joueur(_, S, _), JSBS_post)
                            , member(S, [0, 1, 2])
                            )
                    ) ).

test(perdant) :-
   jeu(J),
   joueurs(JS_full),
   maplist([joueur(A, B), joueur(A, _, B)]>>true, JS, JS_full),
   assertion( forall( ( between(1, 5, _)
                      , random_member(joueur(D, _), JS)
                      , jouer_manche(J, JS, D, _, _, P)
                      )
                    , member(joueur(P, _), JS)
                    ) ).

:- end_tests(jouer_manche).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PARTIE
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Ce programme joue une seule partie.
% Elle se joue en 'nombre_manches/1'.
% À la fin de la partie, l'état des cerveaux de chaque joueur est affiché, sous la forme du terme Prolog construit, ou pas, pendant la partie.
% Cela permet de récupérer cette information pour la réinjecter dans le code !
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%! nombre_manches_par_partie(?N: nat) is det.
%
% Le nombre de manches en lesquelles se joue une partie de "moins de neuf".
%
% @arg N   Le nombre de manches pour jouer une partie de "moins de neuf"
%
% @throws Postcondition.   Le nombre de manches est un entier _strictement_ positif.
%
nombre_manches(200).

:- begin_tests(nombre_manches).

test(strictement_positif) :-
   nombre_manches(N),
   assertion( N > 0 ).

:- end_tests(nombre_manches).

%! presenter_scores(+JTBS: [(joueur, nat, brain)]) is det.
%
% Affichage des scores totaux des joueurs.
%
% @arg JTBS   Liste des joueurs avec leur score de la partie
%
% @throws Precondition.   Les joueurs sont distincts.
%
presenter_scores(JTBS) :-
   assertion( nonvar(JTBS) ),
   findall(J, member(joueur(J, _, _), JTBS), JS),
   % maplist([J, joueur(J, _, _)]>>true, JS, JTBS),
   \+ all_different(JS),
   format("Precondition 'presenter_scores(~w)' violated:  duplicated player\n", [JTBS]),
   assertion( false ).
%
presenter_scores(JTBS) :-
   format("Scores de la partie :\n", []),
   forall( member(joueur(J, S, _), JTBS)
         , format("   - ~w points pour ~w\n", [S, J])
         ).

%! presenter_cerveaux(+JTBS: [(joueur, nat, brain)]) is det.
%
% Affichage des cerveaux finaux des joueurs.
%
% @arg JTBS   Liste des joueurs avec l'état de leur cerveau
%
% @throws Precondition.   Les joueurs sont distincts.
%
presenter_cerveaux(JTBS) :-
   assertion( nonvar(JTBS) ),
   findall(J, member(joueur(J, _, _), JTBS), JS),
   % maplist([J, joueur(J, _, _)]>>true, JS, JTBS),
   \+ all_different(JS),
   format("Precondition 'presenter_cerveaux(~w)' violated:  duplicated player\n", [JTBS]),
   assertion( false ).
%
presenter_cerveaux(JTBS) :-
   format("État des cerveaux après réflexion :\n", []),
   forall( member(joueur(J, _, B), JTBS)
         , format("   - Le cerveau de ~w !\n\n~w\n\n", [J, B])
         ).

%! partie(-JTBS:  [joueur(atom, nat, brain)]]) is det.
%
% Simulation d'une partie de "moins de neuf".
%
% _Nota._ En l'absence de joueur adoptant une stratégie "humaine", la partie se joue entièrement automatiquement !
%
partie(JTBS) :-
   preparer_partie(J, JSBS, D),                                         % On fait les préparatifs : mélanger les cartes, disposer les joueurs autour de la table et choisir le donneur initial.
   findall(joueur(A, 0, B), member(joueur(A, _, B), JSBS), JTBS_zero),  % De plus, on substitue aux stratégies les scores en les initialisant à zéros.
   % maplist([joueur(A, 0, B), joueur(A, _, B)]>>true, JTBS_zero, JSBS),
   nombre_manches(N_M),                                                 % Puis l'on entame une partie en un certain nombre de manches.
   debut_partie(N_M, J, D, JTBS_zero, JTBS).                            % C'est... parti !

%! preparer_partie(-J, -JS, -D) is det.
%
preparer_partie(J_rand, JS_rand, D) :-
   jeu(J),                                                     % On part d'un jeu de cartes neuf,
   full_random_shuffle(J, J_rand),                             % que l'on mélange vigoureusement pour la première fois,
   joueurs(JSBS),                                              % et avec les joueurs connus
   random_permutation(JSBS, JS_rand),                          % que l'on dispose dans un ordre aléatoire autour de la table,
   [joueur(D, _, _) | _] = JS_rand.                            % le "premier" étant le donneur.

%! debut_partie(+N_M, +J, +D, +JTBS, -JTBS_post) is det.
%
debut_partie(0, _, _, JTBS, JTBS) :-                          % Lorsque le nombre de manches restantes est nul, la partie est finie.
   assertion( nonvar(JTBS) ).
debut_partie(N_M, J, D, JTBS, JTBS_post) :-
   assertion( (nonvar(N_M), nonvar(J), nonvar(D), nonvar(JTBS)) ),
   N_M > 0,                                                   % Sinon,
   findall(joueur(A, B), member(joueur(A, _, B), JTBS), JS),
   % maplist([joueur(A, B), joueur(A, _, B)]>>true, JS, JTBS),
   jouer_manche(J, JS, D, J_post, JSBS_manche, P),            % on joue une manche,
   findall( joueur(A, T_inter, B_inter)
          , ( member(joueur(A, T, _), JTBS)
            , member(etat_joueur(A, S, B_inter), JSBS_manche)
            , plus(T, S, T_inter)
            )
          , JTBS_inter
          ),                                                  % on met à jour les scores totaux,
   manual_random_shuffle(J_post, J_shuffle),                  % on mélange de manière "naturelle" le jeu de cartes ramassées,
   plus(N_M_minus_1, 1, N_M),                                 % et on continue avec la manche suivante.
   debut_partie(N_M_minus_1, J_shuffle, P, JTBS_inter, JTBS_post).

%! jouer_partie is det.
%
% Simulation d'une partie de "moins de neuf" sans autre interaction que celles avec les joueurs humains et, en fin de partie, l'affichage des scores de la partie ainsi que l'état des "cerveaux".
%
jouer_partie :-
   partie(JTBS),              % Une fois une partie jouée,
   presenter_scores(JTBS),    % on affiche les scores
   presenter_cerveaux(JTBS).  % ainsi que l'état des cerveaux après avoir joué cette partie (pour les "Intelligences artificielles" qui apprennent).

:- begin_tests(jouer_partie).

test(det) :-
   assertion( forall( between(1, 5, _)
                    , aggregate_all(count, jouer_partie, 1)
                    ) ).

:- end_tests(jouer_partie).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% TESTS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





% :- run_tests().  % Les tests de ce module et éventuellement des modules chargés sont automatiquement exécutés si cette ligne est décommentée.
% :- run_tests(jouer_partie).  % On peut ne lancer que certains tests en précisant lequel ou lesquels (dans une liste).

% :- show_coverage(run_tests). % Lent à très lent, mais permet de savoir à quel point le code a été testé (60 % pour chacun des trois modules en l'état, sachant que certaines parties ne doivent pas s'exécuter quand le code est correct).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% NOTRE IA
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- discontiguous moins_de_neuf:pioche_strategique/7.
:- discontiguous moins_de_neuf:annonce_strategique/7.
:- discontiguous moins_de_neuf:defausse_strategique/8.
:- discontiguous moins_de_neuf:joueur/3.
:- discontiguous moins_de_neuf:strategie/1.

joueur('skynet', skynet, cerveau_vide).

strategie(skynet).

%
% stratégie skynet
% Récap stratégie : Prise en compte des cartes en sommet de pile pour se défausser en anticipant notre pioche. Dépôt de la combinaison / carte avec le maximum de points
%
defausse_strategique(skynet, _, M, _, T3, B, defausse(C_max, P), B) :-
    check_piles_for_combi(T3, M, LSS),                                      % Récupération de toutes les combinaisons possibles avec les cartes en sommets de pile et notre main
    get_max_score_combi(LSS, _, P, CB, _, T3),                              % Récupération de la combinaison avec le nombre de points maximal
    ((  CB \= [],                                                           % Si une combinaison possible a été trouvée
        cartes_combinaison(CBB, CB),                                        % On récupère la liste des cartes formant la combinaison
        remove_elements_liste(CBB, M, M1),                                  % On les supprime de notre main pour les garder au prochain tour pour former la combinaison
        defausse_pioche_opti(M1, C_max)                                     % On se débarrasse de la combinaison / carte avec le plus de points dans notre main triée
    ) ; (                                                                   % S'il n'y aucune combinaison possible
        defausse_pioche_opti(M, C_max)                                      % On se défausse de la carte avec le plus de points dans notre main
    )),
    !.

%
% stratégie skynet
% Récap strategie: On pioche soit la carte qui nous permet de faire la plus grosse combinaison, soit s'il n’y a pas de combinaison possible, la carte la plus faible (si elle est supérieur à, 7 on pioche).
%
pioche_strategique(skynet, _, M, sommets(T1, T2, N_P), B, C, B) :-
   append(T1, T2, CS),                                                 % On concatène les deux piles
   recup_pioche_opti(CS, M, N_P, C),                                   % On récupère la carte à piocher en fonction des paramètres d'entrés
   !.


%
% stratégie skynet
% Récap stratégie : On ne fait l'annonce que si notre somme de points en main est inférieur ou égale à la valeur de la plus petite carte présente sur l'un des deux piles.
% Ce qui permet de ne pas faire d'annonce par exemple si on a 6 points et qu'il y a un As sur l'une des piles. Car le joueur suivent va forcément le prendre et il sera plus dur de le battre.
%
annonce_strategique(skynet, _, M, T3, B, 'moins de neuf', B) :-  % La stratégie skynet s'arrête,
   assertion( (nonvar(M), nonvar(B)) ),
   min_points_piles(T3, VT),                                     % Les points de la carte min sur les piles
   points_cartes(M, VM),                                         % La somme des points de nos cartes,
   VM =< VT,
   !.

annonce_strategique(skynet, _, _, _, B, 'sans annonce', B).
