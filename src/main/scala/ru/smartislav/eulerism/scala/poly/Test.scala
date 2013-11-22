package ru.smartislav.eulerism.scala.poly

import spire.math.Rational

object Test extends App {
  val poly1 = Polynomial(
    Monomial(Rational.one, "x" -> Rational.one, "y" -> Rational(2, 3)),
    Monomial(Rational(4, 5), "y" -> Rational(1, 2), "z" -> Rational(2)))

  val poly2 = Polynomial(
    Monomial(Rational.one, "y" -> Rational(1, 3), "x" -> Rational.one)
  )

  println(s"poly1: $poly1")
  println(s"poly2: $poly2")

  println(s"sum: ${poly1 + poly2}")
  println(s"difference: ${poly1 - poly2}")
  println(s"product: ${poly1 * poly2}")
  println(s"reduction by <poly2>: ${poly1.reduceByBasis(Seq(poly2))}")

  println(s"LT(poly1): ${poly1.lt}")
  println(s"LT(poly2): ${poly2.lt}")

  println(Monomial(Rational.one, "x" -> Rational(2)) isDivisibleBy Monomial(Rational.one, "x" -> Rational.one))

  println(s"Gröbner basis for <poly1, poly2>: ${Polynomial.gröbnerBasis(Seq(poly1, poly2))}")
}
