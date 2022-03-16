import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

import Types.Rational

class TestRational extends AnyFlatSpec {
  "Polynomial" should "be defined" in {
    val r1 = new Rational(4, 2)
    val r2 = new Rational(3, 5)
    
    print(r1)
    print(r2)
    print(r1.plus(r2))

    r1.plus(r2) shouldBe new Rational(13, 5)
  }
}
