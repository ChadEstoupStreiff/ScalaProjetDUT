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
      if (deg < -1)
        times(pow(deg * (-1) - 1).invert()).simplify()
      else if (deg == -1)
        this.invert()
      else if (deg == 0) then
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

    def isPositive(): Boolean = numerateur * denominateur >= 0

    override def equals(rational: Any): Boolean = numerateur == rational.asInstanceOf[Rational].numerateur && denominateur == rational.asInstanceOf[Rational].denominateur

    override def hashCode(): Int = 0

    override def toString: String = numerateur + "/" + denominateur
  }

  class RationalIsFractional extends Fractional[Rational] {
    def fromInt(x: Int): Rational = new Rational(x, 1)

    def minus(x: Rational, y: Rational): Rational = x.minus(y)

    def parseString(str: String): Option[Rational] = Option[Rational] {new Rational(str.split("/")(0).toInt, str.split("/")(1).toInt)}

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

    override def toString: String =
      if(suivant == null) then
        if(deg == 0) then
          a.toString();
        else
          a.toString() + "x^" + deg;
      else
        if(deg == 0) then
          a.toString() + " + " + suivant.toString();
        else
          a.toString() + "x^" + deg + " + " + suivant.toString();

    def eval(x: Rational): Rational =
      if (deg == 0 || suivant == null) then
        a
      else
        a.pow(deg).plus(suivant.eval(suivant.a));

    def concatenate(p : Polynomial): Polynomial =
      if (suivant == null) then
        new Polynomial(p, this.a, this.deg);
      else
        new Polynomial(suivant.concatenate(p), a, deg);

    def remove(p : Polynomial): Polynomial =
      if(suivant == null) then
        if(this.equals(p)) then
          null;
        else
          new Polynomial(null, a, deg);
      else
        if(this.equals(p)) then
          new Polynomial(suivant.suivant, suivant.a, suivant.deg);
        else
          suivant.remove(p);


    def plusSimplePoly(p : Polynomial): Polynomial = //ajoute un simple poly a this
      if(suivant == null) then
        if(this.deg == p.deg)then
          new Polynomial(null, a.plus(p.a), p.deg);
        else
          new Polynomial(p, a.simplify(), deg);
      else if (this.deg == p.deg) then
        suivant.plusSimplePoly(new Polynomial(null, a.plus(p.a), deg));
      else
        new Polynomial(this.suivant.plusSimplePoly(p), a.simplify(), deg);

    def plus(p : Polynomial): Polynomial =
      if(p.suivant == null) then
        this.plusSimplePoly(p);
      else
        this.plusSimplePoly(new Polynomial(null, p.a, p.deg)).plus(p.suivant);

    def simplify(): Polynomial =
      if(suivant == null) then
        new Polynomial(null, a, deg);
      else
        this.remove(new Polynomial(null, a, deg));
        var p = suivant.plusSimplePoly(new Polynomial(null, a, deg));
        p.simplify();

    def minusSimplePoly(p : Polynomial): Polynomial =
      if(suivant == null) then
        if(this.deg == p.deg)then
          new Polynomial(null, a.minus(p.a), p.deg);
        else
          new Polynomial(p, a.simplify(), deg);
      else if (this.deg == p.deg) then
        suivant.minusSimplePoly(new Polynomial(null, a.minus(p.a), deg));
      else
        new Polynomial(this.suivant.minusSimplePoly(p), a.simplify(), deg);

    def minus(p : Polynomial): Polynomial =
      if(p.suivant == null) then
        this.minusSimplePoly(p);
      else
        new Polynomial(this.suivant.minusSimplePoly(p), a.simplify(), deg);

    def timesSimplePoly(p : Polynomial): Polynomial =
      if(suivant == null) then
        new Polynomial(null, a.times(p.a), deg + p.deg)
      else
        new Polynomial(this.suivant.minusSimplePoly(p), a.times(p.a), deg + p.deg);

    def containsSimple(p : Polynomial): Boolean =
      if(suivant == null) then
        a.equals(p.a) && deg == p.deg
      else
        (a.equals(p.a) && deg == p.deg) || suivant.containsSimple(p)

    def contains(p: Polynomial): Boolean =
      if(p.suivant == null) then
        this.containsSimple(p);
      else
        this.containsSimple(p) && this.contains(p.suivant)

    override def equals(p : Any): Boolean =
      this.contains(p.asInstanceOf[Polynomial]) && p.asInstanceOf[Polynomial].contains(this);

  }



  enum ArithExpr:
    case Variable
    case Constant(v: Rational)
    case Neg(a: ArithExpr)
    case Add(left: ArithExpr, right: ArithExpr)
    case Sub(left: ArithExpr, right: ArithExpr)
    case Mult(left: ArithExpr, right: ArithExpr)
    case Div(left: ArithExpr, right: ArithExpr)
    case Pow(left: ArithExpr, exp: ArithExpr)

    def eval(x: Rational): Rational = this match {
      case ArithExpr.Variable => x
      case ArithExpr.Constant(v: Rational) => v
      case ArithExpr.Neg(a: ArithExpr) => a.eval(x).negate()
      case ArithExpr.Add(left: ArithExpr, right: ArithExpr) => left.eval(x).plus(right.eval(x))
      case ArithExpr.Sub(left: ArithExpr, right: ArithExpr) => left.eval(x).minus(right.eval(x))
      case ArithExpr.Mult(left: ArithExpr, right: ArithExpr) => left.eval(x).times(right.eval(x))
      case ArithExpr.Div(left: ArithExpr, right: ArithExpr) => left.eval(x).div(right.eval(x))
      case ArithExpr.Pow(left: ArithExpr, deg: ArithExpr) => left.eval(x).pow(new RationalIsFractional().toInt(deg.eval(x)))
    }

    def lim(x: RationalLimit): RationalLimit = this match {
      case ArithExpr.Variable => x
      case ArithExpr.Constant(v: Rational) => new RationalLimit(false, v.numerateur, v.denominateur)
      case ArithExpr.Neg(a: ArithExpr) => a.lim(x).negate()
      case ArithExpr.Add(left: ArithExpr, right: ArithExpr) => left.lim(x).plus(right.lim(x))
      case ArithExpr.Sub(left: ArithExpr, right: ArithExpr) => left.lim(x).minus(right.lim(x))
      case ArithExpr.Mult(left: ArithExpr, right: ArithExpr) => left.lim(x).times(right.lim(x))
      case ArithExpr.Div(left: ArithExpr, right: ArithExpr) => left.lim(x).div(right.lim(x))
      case ArithExpr.Pow(left: ArithExpr, deg: ArithExpr) => left.lim(x).pow(new RationalIsFractional().toInt(deg.lim(x)))
    }


  class RationalLimit(val infinite: Boolean, numerateur: Int, denominateur: Int) extends Rational(numerateur: Int, denominateur: Int) {
    override def negate(): RationalLimit =
      val l = super.negate(); new RationalLimit(infinite, l.numerateur, l.denominateur)

    override def invert(): RationalLimit =
      val l = super.invert(); new RationalLimit(infinite, l.numerateur, l.denominateur)

    override def plus(rational: Rational): Rational = super.plus(rational.asInstanceOf[RationalLimit])

    def plus(rational: RationalLimit): RationalLimit = infinite match {
      case true => rational.infinite match {
        case true => if (isPositive() == rational.isPositive()) then new RationalLimit(true, numerateur, denominateur) else throw new ArithmeticException("Addition d'infini aux signes contraires")
        case false => new RationalLimit(true, numerateur, denominateur)
      }
      case false => rational.infinite match {
        case true => new RationalLimit(rational.infinite, rational.numerateur, rational.denominateur)
        case false => new RationalLimit(false, numerateur + rational.numerateur, denominateur + rational.denominateur)
      }
    }

    override def minus(rational: Rational): Rational = super.minus(rational.asInstanceOf[RationalLimit])

    def minus(rational: RationalLimit): RationalLimit =  infinite match {
      case true => rational.infinite match {
        case true => if (isPositive() == rational.isPositive()) then new RationalLimit(true, numerateur, denominateur) else throw new ArithmeticException("Addition d'infini aux signes contraires")
        case false => new RationalLimit(true, numerateur, denominateur)
      }
      case false => rational.infinite match {
        case true => new RationalLimit(rational.infinite, rational.numerateur, rational.denominateur)
        case false => new RationalLimit(false, numerateur - rational.numerateur, denominateur - rational.denominateur)
      }
    }

    override def times(rational: Rational): Rational = super.times(rational.asInstanceOf[RationalLimit])

    def times(rational: RationalLimit): RationalLimit = new RationalLimit(infinite || rational.infinite, numerateur * rational.numerateur, denominateur * rational.denominateur)

    override def div(rational: Rational): Rational = super.div(rational.asInstanceOf[RationalLimit])

    def div(rational: RationalLimit): RationalLimit = infinite match {
      case true => rational.infinite match {
        case true => throw new ArithmeticException("Division de deux infinis")
        case false => new RationalLimit(true, numerateur * rational.denominateur, denominateur * rational.numerateur)
      }
      case false => rational.infinite match {
        case true => new RationalLimit(false, 0, 1)
        case false => new RationalLimit(false, numerateur * rational.denominateur, denominateur * rational.numerateur)
      }
    }

    override def pow(deg: Int): RationalLimit = infinite match {
      case true => new RationalLimit(infinite, numerateur, denominateur)
      case false => val l = super.pow(deg); new RationalLimit(infinite, l.numerateur, l.denominateur)
    }

    override def simplify(): RationalLimit = infinite match {
      case true => if isPositive() then new RationalLimit(infinite, 1, 1) else new RationalLimit(infinite, -1, 1)
      case false => val l = super.simplify(); new RationalLimit(infinite, l.numerateur, l.denominateur)
    }

    override def compare(rational: Rational): Int = this.compare(rational.asInstanceOf[RationalLimit])

    def compare(rational: RationalLimit): Int = infinite match {
      case true => if isPositive() then 1 else -1
      case false => rational.infinite match {
        case true => rational.compare(this)
        case false => super.compare(rational)
      }
    }

    override def equals(rational: Any): Boolean = infinite ==
      rational.asInstanceOf[RationalLimit].infinite &&(
      (infinite && isPositive() == rational.asInstanceOf[Rational].isPositive())
      || (!infinite && super.equals(rational)))

    override def toString: String = infinite match {
      case true => if isPositive() then "+inf" else "-inf"
      case false => super.toString()
    }
  }

  class SymbolicFunction(operation: ArithExpr) {
    def eval(x: Rational): Rational = operation.eval(x)
    def lim(x: RationalLimit): RationalLimit = operation.lim(x)
  }
}
