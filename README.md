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
On a décidé de créer une class Rational qui nous permet d'avoir le numérateur et le dénominateur, ce qui nous permet d'être extrémement précis pour les chiffres à virgule ou les rationels qui ne peuvent pas être écris via un double comme 1/3 et donc avoir la valeur exacte.  
Cette classe nous permet d'ajouter certaines fonctions classique d'intéractions entre les rationels ou autre comme plus, minus, times, div, compare etc...
```JAVA
class RationalIsFractional extends Fractional[Rational] {...}
```
Cette classe nous permets d'effectuer les fonctions classique des fractions de base incluses dans Scala comme fromInt, parseString, toFloat etc ...  
Cette classe aurait été particulièrement utile pour les objectifs bonus du projet, que nous n'avons pas eu le temps de faire complétement.
```JAVA
 class RationalLimit(val infinite: Boolean, numerateur: Int, denominateur: Int) extends Rational(numerateur: Int, denominateur: Int) {...}
```
Cette classe permet d'éffectuer des tests limites et de récupérer une limite. Elle permet en réalité de soit avoir un infini ou un rationel.  
Cette classe extends Rational pour avoir toutes les intéractions de ceux ci, mais en plus de ça le booléen *infinite* permet de définir sur c'est un infini ou non, le signe de l'infini est défini par si le rationel est positif ou non.  
On a pu donc implémenter les fonctions d'intéractions des infini et des rationels plus simplement comme plus, minus, times, div, compare etc...
```JAVA
class Polynomial(val suivant: Polynomial, val a: Rational, var deg: Int) {...}
```
Cette classe permet de représenter des polynômes avec des coefficients rationnels. Ils sont représentés sous forme de liste chaîné, on peut donc ajouter des polynômes les uns à la suite des autres. Avec cette classes, il est possible d'ajouter, de soustraire, de multiplier, de diviser, d'évaluer, de dériver et de trouver les la limites de polynômes.
```JAVA
enum ArithExpr: ...
```
Cette enumération est toutes les fonctions arithmétiques implémenté par le projet, grâce au Scala nous pouvons déclarer des types qui auront des arguments, ce qui nous permets de décomposer les fonctions simplement.  
De plus on peut ajouter des fonctions à ces fonctions arithmétique et réagir en fonction de ce qu'elle est.  
On a pu donc définir toutes les évaluations par x, les limites en x et les dérivés des fonctions arithmétiques suivantes:
> Variable  
> Constant(v: Rational)  
> Neg(a: ArithExpr)  
> Add(left: ArithExpr, right: ArithExpr)  
> Sub(left: ArithExpr, right: ArithExpr)  
> Mult(left: ArithExpr, right: ArithExpr)  
> Div(left: ArithExpr, right: ArithExpr)  
> Pow(left: ArithExpr, exp: ArithExpr)  

```JAVA
class SymbolicFunction(operation: ArithExpr) {...}
```
Cette classe est juste une interface permettant de contenire une fonction arithmétique et d'éffectuer les fonctions de base sur la fonction.  
Ici nous pouvons faire l'évaluation de la fonction par x, trouver une limite au point x, ou dériver la fonction.

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
