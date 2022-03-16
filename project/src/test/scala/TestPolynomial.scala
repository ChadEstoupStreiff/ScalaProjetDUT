import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

import Types.*

class TestPolynomial extends AnyFlatSpec{

  "Polynomial" should "be defined" in {
    val r1 = new Rational(2,1)
    val r2 = new Rational(4,2)
    val r3 = new Rational(3,1)
    
    val p3 = new Polynomial(null, r3, 0);
    val p2 = new Polynomial(p3, r2, 1);
    val p1 = new Polynomial(p2, r1, 2);
    
    val x = new Rational(1,1)
    p1.eval(x) shouldBe new Rational(9,1)
  }

}
