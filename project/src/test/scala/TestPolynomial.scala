import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

import Types.*

class TestPolynomial extends AnyFlatSpec{

  "Polynomial" should "be defined" in {
    //eval
    val r1 = new Rational(2,1);
    val r2 = new Rational(4,2);
    val r3 = new Rational(3,1);
    val p3 = new Polynomial(null, r3, 0);
    val p2 = new Polynomial(p3, r2, 1);
    val p1 = new Polynomial(p2, r1, 2);
    //p1(x) = 2*x^2 + 2*x + 3

    val x = new Rational(1,1);
    val expected = new Rational(9,1);
    val actual = p1.eval(x);
    actual.equals(expected) shouldBe true;

    //contains


    //plusSimplePoly
    val p4 = new Polynomial(null, new Rational(2,1),2);
    val pSimpleExp = new Polynomial(new Polynomial(new Polynomial(null, new Rational(3,1), 0), new Rational(2,1), 1), new Rational(4,1), 2);
    val pSimlpleActual = p1.plusSimplePoly(p4);
    pSimlpleActual.equals(pSimpleExp) shouldBe true;


    //plus
    val p7 = new Polynomial(null, new Rational(1,2), 0);
    val p8 = new Polynomial(p7, new Rational(5,1), 2);

    val pExpected  = new Polynomial(new Polynomial(new Polynomial(null, new Rational(7,2), 0), new Rational(2,1), 1), new Rational(7,1), 2);
    val pActual = p1.plus(p8);
    pActual.equals(pExpected) shouldBe true;

    //simplify
    val p5 = new Polynomial(p4, new Rational(2,1),1);
    val p6 = new Polynomial(p5, new Rational(5,1), 1);
    val p6Bis = new Polynomial(p6, new Rational(1,1), 2);

    val pSimplifyExp = new Polynomial(new Polynomial(null, new Rational(3,1), 2), new Rational(7,1), 1);
    val pSimplifyActual = p6Bis.simplify();
    println(p6Bis)
    println(pSimplifyActual);
    println(pSimplifyExp);
    pSimplifyActual.equals(pSimplifyExp) shouldBe true;

    //simple

  }

}
