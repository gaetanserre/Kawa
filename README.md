## Kawa

Compilateur du langage objet `Kawa` vers l'assembleur `MIPS` en OCaml.

Projet final du cours de Langage de programmation et compilation du M1 [MPRI](https://www.universite-paris-saclay.fr/formation/master/informatique/m1-master-parisien-de-recherche-en-informatique-mpri) de Thibaut Balabonski.

## Usage
OCaml, menhir et ocamllex sont nécessaires à la compilation de ce projet.

Il y a un `Makefile` dans le dossier build pour compiler `Kawa`. La commande `make build_tests` compile les programmes `kawa` situés dans le dossier `build`(et ses sous-dossiers) (il faut avoir compilé `Kawa` avant)

### Descripteur de classe
L'adresse de chaque descripteur de classe est stockée comme une variable globale (avec un nom de la forme `descriptor_classname`).
Les instructions nécessaires à initialiser les descripteurs sont exécutées au début du main.

### Héritage
On peut appeler le constructeur de la classe mère de cette manière `this.super([args])`.

### Fonction `main`
Il y a un paramètre "invisible" nommé 'a' dans la fonction main `main`.
Il va prendre la valeur de l'argument passé en paramètre lors de l'exécution du programme.

## `Pimp` vers `MIPS`
## Améliorations de `Pimp`
+ Coloration : s'il n'y a plus de nœuds avec un degrés inférieur au nombre de registre, on choisit celui avec le plus grand degré
+ Optimisation opérations unaires et binaires
+ Ajout des opérateurs $>$, $\leq$, $\geq$, $==$, $/$ et $-$
+ Deadwriteselimination
+ Élimination des moves vers le même registre (`gips2mips`)


### Modifications
+ Ajout d'une instruction de saut ayant pour label le nom de la fonction pour régler le problème du mauvais point d'entrée
+ Correction du code ajoutant les paramètres à la pile lors d'un appel de fonction, le code précédent utilisait les registres virtuels.

### Améliorations possibles
+ Réduire le nombre de `move`
+ Sauvegarder que les registres utilisés par la fonction appelante
