package ru.smartislav.eulerism.scala.poly

import spire.math.Rational

object Test extends App {
  val poly1 = Polynomial(
    Monomial(Rational.one, "x" -> Rational.one, "y" -> Rational(2, 3)),
    Monomial(Rational(4, 5), "y" -> Rational(1, 2), "z" -> Rational(2)))

  val poly2 = Polynomial(
    Monomial(Rational(5, 4), "y" -> Rational(1, 2), "z" -> Rational(2))
  )

  println(s"poly1: $poly1")
  println(s"poly2: $poly2")

  println(s"sum: ${poly1 + poly2}")
  println(s"difference: ${poly1 - poly2}")
  println(s"product: ${poly1 * poly2}")
  println(s"reduction by <poly2>: ${poly1.reduceByBasis(Seq(poly2))}")

  //  println(Monomial(Rational.one, "x" -> Rational(2)) * Monomial(Rational.zero, "y" -> Rational.one))


}
