package ru.smartislav.eulerism.scala.poly

import spire.math.Rational
import scala.collection._
import scala.{Array, Seq}
import scalaz.{Cord, Show}

class Monomial private(val c: Rational, val powers: SortedMap[String, Rational]) extends Ordered[Monomial] {
  def *(factor: Rational): Monomial = Monomial(c * factor, powers)

  def /(factor: Rational): Monomial = Monomial(c / factor, powers)

  def +(other: Monomial): Monomial = {
    if (isZero)
      other
    else if (other.isZero)
      this
    else {
      require(isSimilarTo(other), s"Can't add non-similar monomials $this and $other")
      Monomial(c + other.c, powers)
    }
  }

  def -(other: Monomial): Monomial = {
    if (isZero)
      -other
    else if (other.isZero)
      this
    else {
      require(isSimilarTo(other), s"Can't subtract non-similar monomials $this and $other")
      Monomial(c - other.c, powers)
    }
  }

  def unary_-(): Monomial = {
    Monomial(-c, powers)
  }

  def *(other: Monomial): Monomial = {
    val vars: SortedSet[String] = (powers.map(_._1) ++ other.powers.map(_._1))(breakOut)
    Monomial(c * other.c, (vars map {
      v =>
        v -> (powers.getOrElse(v, Rational.zero) + other.powers.getOrElse(v, Rational.zero))
    }).toSeq: _*)
  }

  def /(other: Monomial): Monomial = {
    require(isDivisibleBy(other))
    val vars = powers.map(_._1) ++ other.powers.map(_._1)
    Monomial(c / other.c, (vars map {
      v =>
        v -> (powers.getOrElse(v, Rational.zero) - other.powers.getOrElse(v, Rational.zero))
    }).toSeq: _*)
  }

  def isSimilarTo(other: Monomial): Boolean = isZero || other.isZero || powers == other.powers

  def isDivisibleBy(other: Monomial): Boolean = {
    other.c != Rational.zero && other.powers.forall({
      case (v, p) => p <= powers.getOrElse(v, Rational.zero)
    })
  }

  def lcm(other: Monomial): Monomial = {
    val vars = powers.map(_._1) ++ other.powers.map(_._1)
    Monomial((c * other.c).abs / c.gcd(other.c), vars.map({
      v => (v, Array(powers.getOrElse(v, Rational.zero), other.powers.getOrElse(v, Rational.zero)).max)
    })(breakOut): SortedMap[String, Rational])
  }

  def isZero: Boolean = this == Monomial.zero

  def nonZero: Boolean = this != Monomial.zero

  def compare(that: Monomial): Int = Monomial.PurelexExactOrdering.compare(this, that)

  override def equals(that: Any): Boolean = {
    if (!that.isInstanceOf[Monomial]) false
    else compare(that.asInstanceOf[Monomial]) == 0
  }

  override def hashCode(): Int = c.hashCode() * 37 + powers.hashCode()

  override def toString: String = Monomial.DebugShow.shows(this)
}

object Monomial {
  val zero = new Monomial(Rational.zero, SortedMap.empty)
  val one = new Monomial(Rational.one, SortedMap.empty)

  def apply(c: Rational, powers: SortedMap[String, Rational]): Monomial = {
    if (c == Rational.zero)
      return Monomial.zero
    val nonZeroPowers = powers.filter(_._2 != Rational.zero)
    if (nonZeroPowers.isEmpty && c == Rational.one)
      return Monomial.one
    require(nonZeroPowers.values.forall(_ > Rational.zero))
    new Monomial(c, nonZeroPowers)
  }

  def apply(c: Rational, powers: (String, Rational)*): Monomial = Monomial(c, SortedMap(powers: _*))


  abstract class ExactOrdering(ord: Ordering[Monomial]) extends Ordering[Monomial] {
    def compare(x: Monomial, y: Monomial): Int = ord.compare(x, y) match {
      case 0 => x.c compareTo y.c
      case r => r
    }
  }

  object PurelexExactOrdering extends ExactOrdering(PurelexSimilarityOrdering)

  implicit val ReversePurelexExactOrdering = PurelexExactOrdering.reverse

  object PurelexSimilarityOrdering extends Ordering[Monomial] {
    def compare(x: Monomial, y: Monomial): Int = {

      val commonVars: SortedSet[String] = x.powers.keySet union y.powers.keySet
      val xPowers: Seq[Rational] = (commonVars map (x.powers.getOrElse(_, Rational.zero)))(breakOut)
      val yPowers: Seq[Rational] = (commonVars map (y.powers.getOrElse(_, Rational.zero)))(breakOut)

      Ordering.Iterable[Rational].compare(xPowers, yPowers)
    }
  }

  val ReversePurelexSimilarityOrdering = PurelexSimilarityOrdering.reverse

  implicit object DebugShow extends Show[Monomial] {
    override def show(m: Monomial): Cord = {
      if (m.c == Rational.zero)
        Cord.empty
      else {
        val coeffCord =
          if (m.c == Rational.one) {
            if (m.powers.isEmpty)
              Cord("1")
            else
              Cord.empty
          } else Cord(m.c.toString, "*")

        val powersCord = Cord.mkCord("*", m.powers.map({
          case (v, p) =>
            if (p == Rational.one)
              Cord(v)
            else if (p.isWhole())
              Cord(v, "^", p.toString)
            else
              Cord(v, "^(", p.toString, ")")
        }).toSeq: _*)
        coeffCord ++ powersCord
      }
    }
  }
}
