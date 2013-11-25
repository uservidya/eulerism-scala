package ru.smartislav.poly

import org.scalacheck.{Gen, Prop}
import spire.math.Rational
import ru.smartislav.SpecBase

class PolynomialSpec extends SpecBase {
  "Addition and subtraction work for all polynomials" ! Prop.forAll(polynomial, polynomial) { (a, b) =>
    (a + b).mustEqual(b + a)
    ((a + b) - b).mustEqual(a)
    (a - b).mustEqual(-(b - a))
  }

  "Zero properties" ! Prop.forAll(polynomial) { (a) =>
    (a - a).mustEqual(Polynomial.zero)
    (a + Polynomial.zero).mustEqual(a)
    (a - Polynomial.zero).mustEqual(a)
    (Polynomial.zero + a).mustEqual(a)
    (Polynomial.zero - a).mustEqual(-a)
  }

  "Any non-zero polynomial is reducible by itself" ! Prop.forAll(nonZeroPolynomial) { (a) =>
    a isReducible a must beTrue
  }

  "Polynomial reduction by any basis terminates" ! Prop.forAll(nonZeroPolynomial, Gen.listOf(nonZeroPolynomial)) { (p, b) =>
    p reduceByBasis b
    true must beTrue
  }

  //  "Check step doesn't crash on any system" ! Prop.forAll(nonZeroPolynomial, Gen.nonEmptyListOf(nonZeroPolynomial),
  //    Gen.nonEmptyListOf(nonZeroPolynomial)) { (f, checked, left) =>
  //    Polynomial.checkOne(f, checked, left)
  //    true must beTrue
  //  }.set(minTestsOk = 10)
  //
  //  "Buchberger's algorithm on any system terminates" ! Prop.forAll(Gen.nonEmptyListOf(nonZeroPolynomial)) { (ps) =>
  //    Polynomial.buchbergersAlgorithm(ps)
  //    true must beTrue
  //  }

  "SPOL example 1" ! {
    // from http://www.scholarpedia.org/article/Groebner_bases (section: Syzygy property)
    val a = Polynomial(
      Monomial("x" -> 1, "y" -> 1),
      Monomial(Rational(2), "x" -> 1),
      Monomial(-Rational.one, "z" -> 1))
    val b = Polynomial(
      Monomial("x" -> 2),
      Monomial(Rational(2), "y" -> 1),
      Monomial(-Rational.one, "z" -> 1))

    val spol = a sPoly b

    spol must beEqualTo(Polynomial(
      Monomial(Rational(2), "x" -> 2),
      Monomial(-Rational.one, "x" -> 1, "z" -> 1),
      Monomial(Rational(-2), "y" -> 2),
      Monomial("y" -> 1, "z" -> 1)))
  }

  "Gröbner basis example 1" ! {
    // correct answer is
    // http://www.wolframalpha.com/input/?i=groebnerbasis%5B%7Bxy+%2B+2x+-+z%2C+x%5E2+%2B+2y+-+z%7D%5D
    // 8 y+8 y^2+2 y^3-4 z-4 y z-y^2 z+z^2, 4 y+2 y^2-2 z+x z-y z, 2 x+x y-z, x^2+2 y-z
    // from http://www.scholarpedia.org/article/Groebner_bases (section: Construction of Gröbner bases)
    val a = Polynomial(
      Monomial("xy"),
      Monomial(2, "x"),
      Monomial(-1, "z"))
    val b = Polynomial(
      Monomial("xx"),
      Monomial(2, "y"),
      Monomial(-1, "z"))

    val basis = Polynomial.gröbnerBasis(Seq(a, b)).reduce().normalize()

    val c = Polynomial(
      Monomial(-1, "xz"),
      Monomial(-2, "yy"),
      Monomial("yz"),
      Monomial(-4, "y"),
      Monomial(2, "z")
    ).normalize()

    val d = Polynomial(
      Monomial(2, "yyy"),
      Monomial(-1, "yyz"),
      Monomial(8, "yy"),
      Monomial(-4, "yz"),
      Monomial(8, "y"),
      Monomial("zz"),
      Monomial(-4, "z")
    )

    basis.dims.sorted mustEqual Seq(a, b, c, d).sorted
    //    false must beTrue
  }
}
