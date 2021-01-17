# Projet Intelligence Artificielle - Le Moins de Neuf

**Auteurs :**
- Lucas HERVOUET
- Étienne LÉCRIVAIN

## Fichiers

Le code est compris dans 2 fichiers distincts :
- **moins_de_neuf.pl** : Contient l'ensemble des fonctions de la stratégie de l'IA.
- **skynet.pl** : Contient l'ensemble des fonctions utiles au fonctionnement de la stratégie de l'IA.

## Stratégies implémentées

### Défausse


### Pioche

On pioche soit la carte qui nous permet de faire la plus grosse combinaison, soit s'il n’y a pas de combinaison possible, la carte la plus faible (si elle est supérieur à, 7 on pioche).

### Annonce

On ne fait l'annonce que si notre somme de points en main est inférieur ou égale à la valeur de la plus petite carte présente sur l'un des deux piles.
Ce qui permet de ne pas faire d'annonce par exemple si on a 6 points et qu'il y a un As sur l'une des piles. 
Car le joueur suivent va forcément le prendre et il sera plus dur de le battre.

## Reste à implémenter

Nous n'avons pas eu le temps d'implémenter des algorithmes utilisant les cerveaux.

