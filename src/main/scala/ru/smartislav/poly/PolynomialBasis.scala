package ru.smartislav.poly

import scala.annotation.tailrec

case class PolynomialBasis(dims: Seq[Polynomial]) {
  def minimize(): PolynomialBasis = {
    val minimized = dims.filter(p => dims.filter(r => p != r).forall(r => !p.isReducible(r)))
    if (dims == minimized)
      this
    else
      PolynomialBasis(minimized)
  }

  def reduceStep(): PolynomialBasis = {
    val reduced = dims.map { d =>
      dims.find { r =>
        val rlpp = r.lpp
        if (r eq d) {
          false
        } else {
          //          println(s"Checking $d against $rlpp")
          d.monomials.exists(m => m.isDivisibleBy(rlpp))
        }
      } map {
        p =>
          d reduce p
      } getOrElse d
    }
    if (dims == reduced)
      this
    else
      PolynomialBasis(reduced)
  }

  def normalize(): PolynomialBasis = {
    PolynomialBasis(dims.map(_.normalize()))
  }

  @tailrec
  final def reduce(): PolynomialBasis = {
    val reduced = minimize().reduceStep()
    if (reduced eq this)
      this
    else
      reduced.reduce()
  }
}
