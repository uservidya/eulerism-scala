package ru.smartislav.eulerism.poly

import org.scalacheck.{Gen, Prop}
import ru.smartislav.eulerism.scala.poly.Polynomial
import ru.smartislav.eulerism.SpecBase

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

  "Buchberger's algorithm on any system terminates" ! Prop.forAll(Gen.nonEmptyListOf(nonZeroPolynomial)) { (ps) =>
  Polynomial.buchbergersAlgorithm(ps)
    true must beTrue
  }
}
