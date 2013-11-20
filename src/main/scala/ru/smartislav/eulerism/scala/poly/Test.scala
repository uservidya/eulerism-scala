package ru.smartislav.eulerism.scala.poly

import spire.math.Rational

object Test extends App {
  val poly = Polynomial(
    Monomial(Rational.one, "x" -> Rational.one, "y" -> Rational(2, 3)),
    Monomial(Rational(4, 5), "y" -> Rational(1, 2), "z" -> Rational(2)))

  println(poly)

  println(Monomial(Rational.one, "x" -> Rational(2)) * Monomial(Rational.zero, "y" -> Rational.one))

  
}
