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

Lors de la défausse, on regarde d'abord, si une carte sur l'une des piles peut nous être utile pour faire une combinaison 
à partir de notre jeu. Si c'est le cas, nous mettons de côté les cartes concernées de notre main pour pouvoir faire 
une combinaison au prochain tour. Également, nous allons nous défausser sur la pile opposée à celle qui contient la 
carte qui nous intéresse. Enfin, nous nous défaussons de la carte où de la combinaison qui nous débarrasse d'un maximum de points.

Si aucune carte ne nous intéresse en sommet de pile. La défausse suit les mêmes étapes, cette fois en prenant en compte
toutes les cartes de la main. La défausse se fait sur la pile opposée à celle qui contient la carte au score minimal, de
façon à ce qu'on puisse la piocher.

### Pioche

On pioche soit la carte qui nous permet de faire la plus grosse combinaison, soit s'il n’y a pas de combinaison possible, la carte la plus faible (si elle est supérieur à, 7 on pioche).

### Annonce

On ne fait l'annonce que si la somme des points des cartes en main est inférieur ou égale à 2.
Ou si la carte maximale de notre main est inférieur ou égale à la carte minimale présente sur l'une des deux piles.
Ce qui permet de ne pas faire d'annonce par exemple si on a 6 points et qu'il y a un As sur l'une des piles. 
Car le joueur suivent va forcément le prendre et il sera plus amène de nous battre.

## Reste à implémenter

Nous n'avons pas eu le temps d'implémenter des algorithmes utilisant les cerveaux.

