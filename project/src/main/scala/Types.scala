import scala.annotation.tailrec

class Rational(val numerateur: Int, val denominateur: Int) {
  def negate(): Rational = new Rational(numerateur * -1, denominateur)

  def invert(): Rational = new Rational(denominateur, numerateur)

  def add(rational: Rational): Rational = new Rational(numerateur * rational.denominateur + rational.numerateur * denominateur, denominateur * rational.denominateur).simplify()

  def sub(rational: Rational): Rational = new Rational(numerateur * rational.denominateur - rational.numerateur * denominateur, denominateur * rational.denominateur).simplify()

  def mult(rational: Rational): Rational = new Rational(numerateur * rational.numerateur, denominateur * rational.denominateur).simplify()

  def div(rational: Rational): Rational = new Rational(numerateur * rational.denominateur, denominateur * rational.numerateur).simplify()

  def simplify(): Rational =
    @tailrec
    def pgcd(a: Int, b: Int): Int =
      if (a == 0) 0
      else if (b == 0) a.abs
      else pgcd(b, a % b)
    val diviseur = pgcd(numerateur, denominateur)
    new Rational(numerateur/diviseur, denominateur/diviseur)

  def compare(rational: Rational): Int =
    if sub(rational).numerateur < 0 then
      -1
    else if sub(rational).numerateur > 0 then
      1
    else
      0

   def equals(rational: Rational): Boolean = numerateur == rational.numerateur && denominateur == rational.denominateur
}

class RationalIsFractional extends Fractional[Rational]{
  def negate(rational: Rational): Rational = new Rational(rational.numerateur * -1, rational.denominateur)

  def invert(): Rational = new Rational(denominateur, numerateur)

  def add(rational: Rational): Rational = new Rational(numerateur * rational.denominateur + rational.numerateur * denominateur, denominateur * rational.denominateur).simplify()

  def sub(rational: Rational): Rational = new Rational(numerateur * rational.denominateur - rational.numerateur * denominateur, denominateur * rational.denominateur).simplify()

  def mult(rational: Rational): Rational = new Rational(numerateur * rational.numerateur, denominateur * rational.denominateur).simplify()

  def div(rational: Rational): Rational = new Rational(numerateur * rational.denominateur, denominateur * rational.numerateur).simplify()

  def simplify(): Rational =
    @tailrec
    def pgcd(a: Int, b: Int): Int =
      if (a == 0) 0
      else if (b == 0) a.abs
      else pgcd(b, a % b)
    val diviseur = pgcd(numerateur, denominateur)
    new Rational(numerateur/diviseur, denominateur/diviseur)

  def compare(rational: Rational): Int =
    if sub(rational).numerateur < 0 then
      -1
    else if sub(rational).numerateur > 0 then
      1
    else
      0

  def equals(rational: Rational): Boolean = numerateur == rational.numerateur && denominateur == rational.denominateur
}

class Polynomial(val a: Rational, val n: Int) {

}


class SymbolicFunction {

}


