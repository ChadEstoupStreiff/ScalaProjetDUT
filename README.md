## Projet Rakowski Maelis & Estoup--Streiff Chad

# Introduction  
Suite à nos cours de programmation fonctionnelle au sein du DUT Informatique, nous avions à développer un projet en Scala.  
Nous avions le choix entre 3 projets:  
```
Sujet 1: interpréteur pour le lambda-calcul  
Sujet 2: calculateur de probabilités pour le poker  
Sujet 3: bibliothèque de calcul symbolique  
```
> github: https://github.com/simonr89/prog_fonctionnelle/wiki  

# Pourquoi ?
Nous avons choisis le projet N°3, alias "***bibliothèque de calcul symbolique***".  
Nous avons choisis ce projet car nous aimons la programmation appliqué aux mathématiques et ayant chacun un bon souvenir d'un projet similaire en modélisation mathématique, ce projet est celui qui nous a le plus tentés.  
De plus nous pensons que ce que nous avons appris lors des cours et des TD serait plus adapté et beaucoup plus utile à des problématique mathématiques.  

# Choix d'implémentations
```JAVA
class Rational(val numerateur: Int, val denominateur: Int) {...}
```
```JAVA
class RationalIsFractional extends Fractional[Rational] {...}
```
```JAVA
 class RationalLimit(val infinite: Boolean, numerateur: Int, denominateur: Int) extends Rational(numerateur: Int, denominateur: Int) {...}
```
```JAVA
class Polynomial(val suivant: Polynomial, val a: Rational, var deg: Int) {...}
```
```JAVA
enum ArithExpr: ...
```
```JAVA
class SymbolicFunction(operation: ArithExpr) {...}
```

# Utile:
Lien vers les ressources dur projet:
> Toutes les ressources: https://github.com/ChadOWGit/ScalaProjetDUT/blob/main/ressources  
> PDF du projet: https://github.com/ChadOWGit/ScalaProjetDUT/blob/main/ressources/projet3.pdf  
> Git des cours: https://github.com/simonr89/prog_fonctionnelle  
> Git du wiki: https://github.com/simonr89/prog_fonctionnelle/wiki  

Lien vers le code:
> SRC: https://github.com/ChadOWGit/ScalaProjetDUT/tree/main/project/src  
> Types: https://github.com/ChadOWGit/ScalaProjetDUT/blob/main/project/src/main/scala/Types.scala  

Lien vers les tests:
> Tous les tests: https://github.com/ChadOWGit/ScalaProjetDUT/tree/main/project/src/test/scala  
> Test Rational: https://github.com/ChadOWGit/ScalaProjetDUT/blob/main/project/src/test/scala/TestRational.scala  
> Test Polynomes: https://github.com/ChadOWGit/ScalaProjetDUT/blob/main/project/src/test/scala/TestPolynomial.scala  
> Test Symbolic: https://github.com/ChadOWGit/ScalaProjetDUT/blob/main/project/src/test/scala/TestSymbolicFunction.scala  