import scala.annotation.tailrec
object Types {
  class Rational(val numerateur: Int, val denominateur: Int) {
    def copy(): Rational = new Rational(numerateur, denominateur)

    def negate(): Rational = new Rational(numerateur * -1, denominateur)

    def invert(): Rational = new Rational(denominateur, numerateur)

    def plus(rational: Rational): Rational = new Rational(numerateur * rational.denominateur + rational.numerateur * denominateur, denominateur * rational.denominateur).simplify()

    def minus(rational: Rational): Rational = new Rational(numerateur * rational.denominateur - rational.numerateur * denominateur, denominateur * rational.denominateur).simplify()

    def times(rational: Rational): Rational = new Rational(numerateur * rational.numerateur, denominateur * rational.denominateur).simplify()

    def div(rational: Rational): Rational = new Rational(numerateur * rational.denominateur, denominateur * rational.numerateur).simplify()

    def pow(deg: Int): Rational =
      if (deg < -1)
        invert().pow(-deg)
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
      if (diviseur != 0) then new Rational(numerateur / diviseur, denominateur / diviseur) else new Rational(numerateur, denominateur)

    def compare(rational: Rational): Int =
      val l = minus(rational);
      if l.isPositive() then
        if l.numerateur * l.denominateur == 0 then 0 else 1
      else -1

    def isPositive(): Boolean = numerateur * denominateur >= 0

    override def equals(rational: Any): Boolean = rational.asInstanceOf[Rational].denominateur * numerateur / denominateur == rational.asInstanceOf[Rational].numerateur

    override def hashCode(): Int = 0

    override def toString: String = if denominateur != 1 then numerateur + "/" + denominateur else numerateur.toString
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

  class RationalLimit(val infinite: Boolean, numerateur: Int, denominateur: Int) extends Rational(numerateur: Int, denominateur: Int) {
    override def negate(): RationalLimit =
      val l = super.negate(); new RationalLimit(infinite, l.numerateur, l.denominateur)

    override def invert(): RationalLimit =
      val l = super.invert(); new RationalLimit(infinite, l.numerateur, l.denominateur)

    override def plus(rational: Rational): Rational = super.plus(new RationalLimit(false, rational.numerateur, rational.denominateur))

    def plus(rational: RationalLimit): RationalLimit = infinite match {
      case true => rational.infinite match {
        case true => if (isPositive() == rational.isPositive()) then new RationalLimit(true, numerateur, denominateur) else throw new ArithmeticException("Addition d'infini aux signes contraires")
        case false => new RationalLimit(true, numerateur, denominateur)
      }
      case false => rational.infinite match {
        case true => new RationalLimit(rational.infinite, rational.numerateur, rational.denominateur)
        case false => new RationalLimit(false, numerateur*rational.denominateur + rational.numerateur*denominateur, denominateur * rational.denominateur)
      }
    }

    override def minus(rational: Rational): Rational = super.minus(new RationalLimit(false, rational.numerateur, rational.denominateur))

    def minus(rational: RationalLimit): RationalLimit =  infinite match {
      case true => rational.infinite match {
        case true => if (isPositive() == rational.isPositive()) then new RationalLimit(true, numerateur, denominateur) else throw new ArithmeticException("Addition d'infini aux signes contraires")
        case false => new RationalLimit(true, numerateur, denominateur)
      }
      case false => rational.infinite match {
        case true => new RationalLimit(rational.infinite, rational.numerateur, rational.denominateur)
        case false => new RationalLimit(false, numerateur*rational.denominateur - rational.numerateur*denominateur, denominateur * rational.denominateur)
      }
    }

    override def times(rational: Rational): Rational = super.times(new RationalLimit(false, rational.numerateur, rational.denominateur))

    def times(rational: RationalLimit): RationalLimit = new RationalLimit(infinite || rational.infinite, numerateur * rational.numerateur, denominateur * rational.denominateur)

    override def div(rational: Rational): Rational = super.div(new RationalLimit(false, rational.numerateur, rational.denominateur))

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
      case true => if deg == 0 then new RationalLimit(false, 1, 1) else if deg > 0 then new RationalLimit(infinite, numerateur, denominateur) else new RationalLimit(infinite, -numerateur, denominateur)
      case false => val l = super.pow(deg); new RationalLimit(infinite, l.numerateur, l.denominateur)
    }

    override def simplify(): RationalLimit = infinite match {
      case true => if isPositive() then new RationalLimit(infinite, 1, 1) else new RationalLimit(infinite, -1, 1)
      case false => val l = super.simplify(); new RationalLimit(infinite, l.numerateur, l.denominateur)
    }

    override def compare(rational: Rational): Int = this.compare(new RationalLimit(false, rational.numerateur, rational.denominateur))

    def compare(rational: RationalLimit): Int = infinite match {
      case true => rational.infinite match {
        case true => if isPositive() == rational.isPositive() then 0 else if isPositive() then 1 else -1
        case false => if isPositive() then 1 else -1
      }
      case false => rational.infinite match {
        case true => if rational.isPositive() then -1 else 1
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

  class Polynomial(val suivant: Polynomial, val a: Rational, var deg: Int) {

    def copy(): Polynomial =
      if suivant == null then
        new Polynomial(null, a.copy(), deg)
      else
        new Polynomial(suivant.copy(), a.copy(), deg)

    override def toString: String =
      if (suivant == null) then
        if (deg == 0) then
          a.toString();
        else
          a.toString() + "x^" + deg;
      else if (deg == 0) then
        a.toString() + " + " + suivant.toString();
      else
        a.toString() + "x^" + deg + " + " + suivant.toString();

    def toArith(): ArithExpr =
      if (suivant == null)
        ArithExpr.Mult(ArithExpr.Constant(a.copy()), ArithExpr.Pow(ArithExpr.Variable, ArithExpr.Constant(new Rational(deg, 1))))
      else
        ArithExpr.Add(ArithExpr.Mult(ArithExpr.Constant(a.copy()), ArithExpr.Pow(ArithExpr.Variable, ArithExpr.Constant(new Rational(deg, 1)))), suivant.toArith())

    def eval(x: Rational): Rational =
      if (suivant == null) then
        a.times(x.pow(deg))
      else
        a.times(x.pow(deg)).plus(suivant.eval(suivant.a));

    def concatenate(p : Polynomial): Polynomial =
      if (suivant == null) then
        new Polynomial(p.copy(), this.a.copy(), this.deg);
      else
        new Polynomial(suivant.concatenate(p), a.copy(), deg);

    def plusSimplePoly(p : Polynomial): Polynomial = //ajoute un simple poly a this
      if(suivant == null) then
        if(this.deg == p.deg)then
          new Polynomial(null, a.plus(p.a), p.deg);
        else
          new Polynomial(new Polynomial(null, p.a.copy(), p.deg), a.simplify(), deg);
      else if (this.deg == p.deg) then
        new Polynomial(suivant.copy(), a.plus(p.a), deg);
      else
        new Polynomial(this.suivant.plusSimplePoly(p), a.simplify(), deg);

    def plus(p : Polynomial): Polynomial =
      if(p.suivant == null) then
        this.plusSimplePoly(p);
      else
        this.plusSimplePoly(new Polynomial(null, p.a.copy(), p.deg)).plus(p.suivant);

    def minusSimplePoly(p : Polynomial): Polynomial =
      if(suivant == null) then
        if(this.deg == p.deg)then
          new Polynomial(null, a.minus(p.a), deg);
        else
          new Polynomial(new Polynomial(null, p.a.copy(), p.deg), a.simplify(), deg);
      else if (this.deg == p.deg) then
        new Polynomial(suivant.copy(), a.minus(p.a), deg);
      else
        new Polynomial(this.suivant.minusSimplePoly(p), a.simplify(), deg);

    def minus(p : Polynomial): Polynomial =
      if(p.suivant == null) then
        this.minusSimplePoly(p);
      else
        this.minusSimplePoly(new Polynomial(null, p.a.copy(), p.deg)).minus(p.suivant);

    def simplify(): Polynomial =
      if (suivant == null) then
        copy()
      else
        suivant.simplify().plusSimplePoly(this)

    //TODO
    def timesSimplePoly(p : Polynomial): Polynomial =
      if(suivant == null) then
        new Polynomial(null, a.times(p.a), deg + p.deg)
      else
        new Polynomial(this.suivant.minusSimplePoly(p), a.times(p.a), deg + p.deg);

    //TODO
    def times(p: Polynomial): Polynomial =
      if(p.suivant == null) then
        this.timesSimplePoly(p);
      else
        new Polynomial(this.suivant.timesSimplePoly(p), a.simplify(), deg).simplify();

    def limitSimplePoly(): RationalLimit =
      if (deg == 0) then
        new RationalLimit(false, a.numerateur, a.denominateur);
      else if (a.numerateur == 0) then
        new RationalLimit(false, 0, 0);
      else
        new RationalLimit(true, a.numerateur, a.denominateur);

    //TODO les tests de limit
    def limit(plusGrand: Polynomial): RationalLimit =
      if (suivant == null) then
        if (plusGrand.deg < this.deg) then
          this.limitSimplePoly();
        else if (plusGrand.deg > this.deg) then
          plusGrand.limitSimplePoly();
        else if (plusGrand.a.compare(this.a) < 0) then
          this.limitSimplePoly();
        else
          plusGrand.limitSimplePoly();
      else if (plusGrand.deg < this.deg) then
        suivant.limit(this);
      else if (plusGrand.deg > this.deg) then
        suivant.limit(plusGrand)
      else if (plusGrand.a.compare(this.a) < 0) then
        suivant.limit(this);
      else
        suivant.limit(plusGrand)

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
    // Fct that can be add that would have been usefull
    // case Log(a: ArithExpr)
    // case cos(a: ArithExpr)
    // case sin(a: ArithExpr)
    // case tan(a: ArithExpr)

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
      case ArithExpr.Pow(left: ArithExpr, deg: ArithExpr) =>
        val c = left.lim(x);
        val l = deg.lim(x);
        l.infinite match {
          case true =>
            if c.compare(new Rational(-1, 1)) == 1 && c.compare(new Rational(1, 1)) == -1 then new RationalLimit(false, 0, 1)
            else if c.equals(new RationalLimit(false, 1, 1)) then new RationalLimit(false, 1, 1)
            else if c.isPositive() then new RationalLimit(true, 1, 1)
            else throw new ArithmeticException("Puissance infini d'une fonction <= -1")
          case false => if c.infinite then if l.isPositive() then new RationalLimit(true, 1, 1) else new RationalLimit(true, -1, 1) else left.lim(x).pow(new RationalIsFractional().toInt(deg.lim(x)))
        }
    }

    def derivate(): ArithExpr = this match {
      case ArithExpr.Variable => ArithExpr.Constant(new Rational(1, 1))
      case ArithExpr.Constant(v: Rational) => ArithExpr.Constant(new Rational(0, 1))
      case ArithExpr.Neg(a: ArithExpr) => ArithExpr.Neg(a.derivate())
      case ArithExpr.Add(left: ArithExpr, right: ArithExpr) => ArithExpr.Add(left.derivate(), right.derivate())
      case ArithExpr.Sub(left: ArithExpr, right: ArithExpr) => ArithExpr.Sub(left.derivate(), right.derivate())
      case ArithExpr.Mult(left: ArithExpr, right: ArithExpr) =>
        if left == ArithExpr.Constant then ArithExpr.Mult(left, right.derivate())
        else if right == ArithExpr.Constant then ArithExpr.Mult(right, left.derivate())
        else ArithExpr.Add(ArithExpr.Mult(left.derivate(), right), ArithExpr.Mult(left, right.derivate()))
      case ArithExpr.Div(left: ArithExpr, right: ArithExpr) => ArithExpr.Div(ArithExpr.Sub(ArithExpr.Mult(left.derivate(), right), ArithExpr.Mult(left, right.derivate())), ArithExpr.Mult(right, right))
      case ArithExpr.Pow(left: ArithExpr, deg: ArithExpr) => ??? // vu^(v−1u)′+log(u)u^(v)v′ => need cos fct
    }

    override def toString: String = this match {
      case ArithExpr.Variable => "x"
      case ArithExpr.Constant(v: Rational) => v.toString
      case ArithExpr.Neg(a: ArithExpr) => "-" + a.toString
      case ArithExpr.Add(left: ArithExpr, right: ArithExpr) => left.toString + " + " + right.toString
      case ArithExpr.Sub(left: ArithExpr, right: ArithExpr) => left.toString + " - " + right.toString
      case ArithExpr.Mult(left: ArithExpr, right: ArithExpr) => "(" + left.toString + ")*(" + right.toString + ")"
      case ArithExpr.Div(left: ArithExpr, right: ArithExpr) => "(" + left.toString + ")/(" + right.toString + ")"
      case ArithExpr.Pow(left: ArithExpr, deg: ArithExpr) => "(" + left.toString + ")^(" + deg.toString + ")"
    }

  class SymbolicFunction(operation: ArithExpr) {
    def eval(x: Rational): Rational = operation.eval(x)
    def lim(x: RationalLimit): RationalLimit = operation.lim(x)
    def derivate(): SymbolicFunction = new SymbolicFunction(operation.derivate())

    override def toString: String = operation.toString
  }
}
