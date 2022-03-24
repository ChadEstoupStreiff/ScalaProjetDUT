import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

import Types.Rational
import Types.RationalIsFractional

class TestRational extends AnyFlatSpec {

  "Equals" should "be defined" in {
    val r1 = new Rational(4, 2)
    val r2 = new Rational(3, 5)
    val r3 = new Rational(24, 24)
    val r4 = new Rational(30, 20)

    r1 shouldBe new Rational(8, 4)
    r2 shouldBe new Rational(12, 20)
    r3 shouldBe new Rational(12, 12)
    r4 shouldBe new Rational(3, 2)
  }

  "Simplify" should "be defined" in {
    val r1 = new Rational(4, 2)
    val r2 = new Rational(3, 5)
    val r3 = new Rational(24, 24)
    val r4 = new Rational(30, 20)

    r1.simplify() shouldBe new Rational(2, 1)
    r2.simplify() shouldBe new Rational(3, 5)
    r3.simplify() shouldBe new Rational(1, 1)
    r4.simplify() shouldBe new Rational(3, 2)
  }

  "Invert" should "be defined" in {
    val r1 = new Rational(4, 2)
    val r2 = new Rational(3, 5)
    val r3 = new Rational(8, 5)

    r1.invert() shouldBe new Rational(2, 4)
    r2.invert() shouldBe new Rational(5, 3)
    r3.invert() shouldBe new Rational(5, 8)
  }

  "Negate" should "be defined" in {
    val r1 = new Rational(4, 2)
    val r2 = new Rational(3, 5)
    val r3 = new Rational(8, 5)

    r1.negate() shouldBe new Rational(-4, 2)
    r2.negate() shouldBe new Rational(-3, 5)
    r3.negate() shouldBe new Rational(-8, 5)
  }

  "Add" should "be defined" in {
    val r1 = new Rational(4, 2)
    val r2 = new Rational(3, 5)
    val r3 = new Rational(8, 5)

    r1.plus(r2) shouldBe new Rational(13, 5)
    r2.plus(r3) shouldBe new Rational(11, 5)
    r3.plus(r1) shouldBe new Rational(18, 5)
  }

  "Sub" should "be defined" in {
    val r1 = new Rational(4, 2)
    val r2 = new Rational(3, 5)
    val r3 = new Rational(8, 5)

    r1.minus(r2) shouldBe new Rational(7, 5)
    r2.minus(r3) shouldBe new Rational(-1, 1)
    r3.minus(r1) shouldBe new Rational(-2, 5)
  }

  "mult" should "be defined" in {
    val r1 = new Rational(4, 2)
    val r2 = new Rational(3, 5)
    val r3 = new Rational(8, 5)

    r1.times(r2) shouldBe new Rational(6, 5)
    r2.times(r3) shouldBe new Rational(24, 25)
    r3.times(r1) shouldBe new Rational(16, 5)
  }

  "div" should "be defined" in {
    val r1 = new Rational(4, 2)
    val r2 = new Rational(3, 5)
    val r3 = new Rational(8, 5)

    r1.div(r2) shouldBe new Rational(10, 3)
    r2.div(r3) shouldBe new Rational(3, 8)
    r3.div(r1) shouldBe new Rational(4, 5)
  }

  "pow" should "be defined" in {
    val r1 = new Rational(2, 3)

    r1.pow(3) shouldBe new Rational(8, 27)
    r1.pow(-3) shouldBe new Rational(27, 8)
    r1.pow(2) shouldBe new Rational(4, 9)
  }

  "parseString" should "be defined" in {
    new RationalIsFractional().parseString("2/5") shouldBe Option[Rational] {new Rational(2, 5)}
    new RationalIsFractional().parseString("2/15") shouldBe Option[Rational] {new Rational(2, 15)}
    new RationalIsFractional().parseString("13/5") shouldBe Option[Rational] {new Rational(13, 5)}
    new RationalIsFractional().parseString("12/55") shouldBe Option[Rational] {new Rational(12, 55)}
  }
}
