[//]:#(gvim: set fileencoding=utf8)

# Le << Moins de neuf >>

## Contenu

Ce répertoire contient trois fichiers de codes sources Prolog :

- "utilities.pl" ;
- "quickcheck.pl" ;
- "moins_de_neuf.pl".

## Utilitaires

Les prédicats utilitaires sont une petite collection de prédicats d'usage général.
Tous ne sont pas utilisés par le jeu.
Ils s'accumulent au fur et à mesure des besoins, sans autre logique que d'être utilisables ou utilisés dans différents projets.

## Tests à la QuickCheck

Le fichier "quickcheck.pl" contient un certain nombre de prédicats qui ont en commun de générer des données aléatoires.
Il émule de manière extrêmement rudimentaire la bibliothèque QuickCheck du langage Haskell.
Son principe de fonctionnement est de générer des test avec des données aléatoires.
La façon de s'approcher autant que possible du véritable QuickCheck est présentée dans le fichier lui-même.

## Jeu

Le fichier du jeu contient toute la mécanique de gestion des interactions, dans le respect des règles.
Il peut sembler impressionnant de prime abord avec plus de 4 000 lignes de codes.
La version "nettoyée" et minimale n'atteint que 500 lignes.

_Nota._ le code ne contient pas le dernier tour de jeu après l'annonce.
Cela est volontaire car la réflexion à mener dans cette étape finale est sans doute assez différente des tours précédents.
Elle peut être ajoutée si les agents ont été conçus pour cela.

