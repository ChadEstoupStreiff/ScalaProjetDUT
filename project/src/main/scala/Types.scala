import scala.annotation.tailrec
object Types {
  class Rational(val numerateur: Int, val denominateur: Int) {
    def negate(): Rational = new Rational(numerateur * -1, denominateur)

    def invert(): Rational = new Rational(denominateur, numerateur)

    def plus(rational: Rational): Rational = new Rational(numerateur * rational.denominateur + rational.numerateur * denominateur, denominateur * rational.denominateur).simplify()

    def minus(rational: Rational): Rational = new Rational(numerateur * rational.denominateur - rational.numerateur * denominateur, denominateur * rational.denominateur).simplify()

    def times(rational: Rational): Rational = new Rational(numerateur * rational.numerateur, denominateur * rational.denominateur).simplify()

    def div(rational: Rational): Rational = new Rational(numerateur * rational.denominateur, denominateur * rational.numerateur).simplify()

    def pow(deg: Int): Rational =
      if (deg == 0) then
        new Rational(1, 1)
      else if (deg == 1) then
        this
      else
        times(pow(deg - 1)).simplify();

    def simplify(): Rational =
      @tailrec
      def pgcd(a: Int, b: Int): Int =
        if (a == 0) 0
        else if (b == 0) a.abs
        else pgcd(b, a % b)

      val diviseur = pgcd(numerateur, denominateur)
      new Rational(numerateur / diviseur, denominateur / diviseur)

    def compare(rational: Rational): Int =
      if minus(rational).numerateur < 0 then
        -1
      else if minus(rational).numerateur > 0 then
        1
      else
        0

    def equals(rational: Rational): Boolean = numerateur == rational.numerateur && denominateur == rational.denominateur

    override def hashCode(): Int = 0

    override def toString: String = numerateur + "/" + denominateur
  }

  class RationalIsFractional extends Fractional[Rational] {
    def fromInt(x: Int): Rational = new Rational(x, 1)

    def minus(x: Rational, y: Rational): Rational = x.minus(y)

    def parseString(str: String): Option[Rational] = ???

    def plus(x: Rational, y: Rational): Rational = x.plus(y)

    def times(x: Rational, y: Rational): Rational = x.times(y)

    def toDouble(x: Rational): Double = x.numerateur / x.denominateur

    def toFloat(x: Rational): Float = x.numerateur / x.denominateur

    def toInt(x: Rational): Int = x.numerateur / x.denominateur

    def toLong(x: Rational): Long = x.numerateur / x.denominateur

    def div(x: Rational, y: Rational): Rational = x.div(y)

    def negate(x: Rational): Rational = x.negate()

    def compare(x: Rational, y: Rational): Int = x.compare(y)
  }

  class Polynomial(val suivant: Polynomial, val a: Rational, var deg: Int) {
    def eval(x: Rational): Rational =
      if (deg == 0 || suivant.equals(null)) then
        a.times(x);
      else
        a.pow(deg).plus(suivant.eval(suivant.a));
  }


  enum ArithExpr:
    case Variable
    case Constant(v: Rational)
    //case Neg(e: ArithExpr)
    case Add
    case Sub
    case Mult


  class SymbolicFunction(operation: ArithExpr, left: SymbolicFunction, right: SymbolicFunction) {
    def eval(x: Rational): Rational = operation match {
      case ArithExpr.Variable => x
      case ArithExpr.Constant(v: Rational) => v
      case ArithExpr.Add => left.eval(x).plus(right.eval(x))
      case ArithExpr.Sub => left.eval(x).minus(right.eval(x))
      case ArithExpr.Mult => left.eval(x).times(right.eval(x))
    }
  }
}
