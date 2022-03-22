import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

import Types.*

class TestSymbolicFunction extends AnyFlatSpec {

  "Eval" should "be defined" in {
    // (2+x)*3/2
    val f1 = new SymbolicFunction(ArithExpr.Mult(ArithExpr.Add(ArithExpr.Constant(new Rational(2, 1)), ArithExpr.Variable), ArithExpr.Constant(new Rational(3, 2))))
    // 3(x+2)^x
    val f2 = new SymbolicFunction(ArithExpr.Mult(ArithExpr.Constant(new Rational(3, 1)), ArithExpr.Pow(ArithExpr.Add(ArithExpr.Variable, ArithExpr.Constant(new Rational(2, 1))), ArithExpr.Variable)))


    f1.eval(new Rational(3, 1)) shouldBe new Rational(15, 2)
    f1.eval(new Rational(3, 2)) shouldBe new Rational(21, 4)

    f2.eval(new Rational(-1, 1)) shouldBe new Rational(3, 1)
    f2.eval(new Rational(1, 1)) shouldBe new Rational(9, 1)
  }
}
