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

    val x = new Rational(1,1);
    val expected = new Rational(9,1);
    val actual = p1.eval(x);
    println( expected.numerateur + " / " + expected.denominateur );
    println( actual.numerateur+ " / " + actual.denominateur);
    actual.equals(expected) shouldBe true;

    //contains


    //plusSimplePoly
    val p4 = new Polynomial(null, new Rational(2,1),2);
    val pSimpleExp = new Polynomial(new Polynomial(new Polynomial(null, new Rational(3,1), 0), new Rational(2,1), 1), new Rational(4,1), 2);
    val pSimlpleActual = p1.plusSimplePoly(p4);
    println(pSimlpleActual);
    println(pSimpleExp);
    pSimlpleActual.equals(pSimpleExp) shouldBe true;

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


    //plus
    val p7 = new Polynomial(null, new Rational(1,2), 0);
    val p8 = new Polynomial(p7, new Rational(5,1), 2);

    val pExpected  = new Polynomial(new Polynomial(new Polynomial(null, new Rational(7,2), 0), new Rational(2,1), 1), new Rational(7,1), 2);
    val pActual = p1.plus(p8);
    println(pActual);
    println(pExpected);
    pActual.equals(pExpected) shouldBe true;

    //limite
    val limitExp = new RationalLimit(true, 2, 1);
    val limitActual = p4.limitSimplePoly();
    println("limitAcual : " +limitActual);
    println("limitExp : " + limitExp);
    limitActual.equals(limitExp) shouldBe true;

    //derivee
    val deriveeExp = new Polynomial(null, new Rational(10,1), 1);
    val deriveeActual = p8.derivee();
    println("deriveeActual : " +deriveeActual);
    println("deriveeExp : " + deriveeExp);
    deriveeActual.equals(deriveeExp) shouldBe true;

    val derivee2Exp = new Polynomial(new Polynomial(null, new Rational(7,1), 0), new Rational(6,1), 1);
    val derivee2Actual = p6Bis.derivee();
    println("derivee2Actual : " +derivee2Actual);
    println("derivee2Exp : " + derivee2Exp);
    derivee2Actual.equals(derivee2Exp) shouldBe true;
  }

}
