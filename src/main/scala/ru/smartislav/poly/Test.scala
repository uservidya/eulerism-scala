package ru.smartislav.poly

import spire.math.Rational

object Test extends App {
  val poly1 = Polynomial(
    Monomial(Rational.one, "x" -> 3, "y" -> 2),
    Monomial(Rational(4, 5), "y" -> 1, "z" -> 3))

  val poly2 = Polynomial(
    Monomial(Rational.one, "y" -> 1, "x" -> 2)
  )

  println(s"poly1: $poly1")
  println(s"poly2: $poly2")

  println(s"sum: ${poly1 + poly2}")
  println(s"difference: ${poly1 - poly2}")
  println(s"product: ${poly1 * poly2}")
  println(s"reduction by <poly2>: ${poly1.reduceByBasis(Seq(poly2))}")

  println(s"LT(poly1): ${poly1.lpp }")
  println(s"LT(poly2): ${poly2.lpp }")

  println(Monomial(Rational.one, "x" -> 2) isDivisibleBy Monomial(Rational.one, "x" -> 1))

  println(s"Gröbner basis for <poly1, poly2>: ${Polynomial.gröbnerBasis(Seq(poly1, poly2))}")


  val a = Polynomial(
    Monomial("x" -> 1, "y" -> 1),
    Monomial(Rational(2), "x" -> 1),
    Monomial(-Rational.one, "z" -> 1))
  val b = Polynomial(
    Monomial("x" -> 2),
    Monomial(Rational(2), "y" -> 1),
    Monomial(-Rational.one, "z" -> 1))

  val basis = Polynomial.gröbnerBasis(Seq(a, b)).reduce()

  val c = Polynomial(
    Monomial(-Rational.one, "x" -> 1, "z" -> 1),
    Monomial(Rational(-2), "y" -> 2),
    Monomial("y" -> 1, "z" -> 1),
    Monomial(Rational(-4), "y" -> 1),
    Monomial(Rational(2), "z" -> 1)
  )

  println(s"Reduced Gröbner Basis: $basis")
}
