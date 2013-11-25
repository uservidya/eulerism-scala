package ru.smartislav.poly

import spire.math.Rational
import scala.collection._
import scala.{Array, Seq}
import scalaz.{Cord, Show}
import ru.smartislav.eulerism

class Monomial private(val c: Rational, val powers: SortedMap[String, Int]) extends Ordered[Monomial] {
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
        v -> (powers.getOrElse(v, 0) + other.powers.getOrElse(v, 0))
    }).toSeq: _*)
  }

  def /(other: Monomial): Monomial = {
    require(isDivisibleBy(other))
    val vars = powers.map(_._1) ++ other.powers.map(_._1)
    Monomial(c / other.c, (vars map {
      v =>
        v -> (powers.getOrElse(v, 0) - other.powers.getOrElse(v, 0))
    }).toSeq: _*)
  }

  def isSimilarTo(other: Monomial): Boolean = isZero || other.isZero || powers == other.powers

  def isDivisibleBy(other: Monomial): Boolean = {
    other.c != Rational.zero && other.powers.forall({
      case (v, p) => p <= powers.getOrElse(v, 0)
    })
  }

  def lcm(other: Monomial): Monomial = {
    val vars = powers.map(_._1) ++ other.powers.map(_._1)
    Monomial((c * other.c).abs / c.gcd(other.c), vars.map({
      v => (v, Array(powers.getOrElse(v, 0), other.powers.getOrElse(v, 0)).max)
    })(breakOut): SortedMap[String, Int])
  }

  def coprimeUpToCoeff(other: Monomial): Boolean = {
    (powers.keySet intersect other.powers.keySet).isEmpty
  }

  def isZero: Boolean = this == Monomial.zero

  def nonZero: Boolean = this != Monomial.zero

  def powerProduct: Monomial = new Monomial(Rational.one, powers)

  def compare(that: Monomial): Int = Monomial.DefaultOrdering.compare(this, that)

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

  def apply(c: Rational, powers: SortedMap[String, Int]): Monomial = {
    if (c == Rational.zero)
      return Monomial.zero
    val nonZeroPowers = powers.filter(_._2 != 0)
    if (nonZeroPowers.isEmpty && c == Rational.one)
      return Monomial.one
    require(nonZeroPowers.values.forall(_ > 0))
    new Monomial(c, nonZeroPowers)
  }

  def apply(c: Rational, powers: (String, Int)*): Monomial = Monomial(c, SortedMap(powers: _*))

  def apply(powers: (String, Int)*): Monomial = Monomial(Rational.one, powers: _*)

  def apply(c: Rational, s: String): Monomial = {
    Monomial(c, eulerism.groupRuns(s.sorted[Char].iterator) { (c) =>
      c.head.toString -> c.length
    }: _*)
  }

  def apply(s: String): Monomial = Monomial(Rational.one, s)

  abstract class ExactOrdering(ord: Ordering[Monomial]) extends Ordering[Monomial] {
    def compare(x: Monomial, y: Monomial): Int = ord.compare(x, y) match {
      case 0 => x.c compareTo y.c
      case r => r
    }
  }

  object PurelexExactOrdering extends ExactOrdering(PurelexSimilarityOrdering)

  val ReversePurelexExactOrdering = PurelexExactOrdering.reverse

  object PurelexSimilarityOrdering extends Ordering[Monomial] {
    def compare(x: Monomial, y: Monomial): Int = {

      val commonVars: SortedSet[String] = x.powers.keySet union y.powers.keySet
      val xPowers: Seq[Int] = (commonVars map (x.powers.getOrElse(_, 0)))(breakOut)
      val yPowers: Seq[Int] = (commonVars map (y.powers.getOrElse(_, 0)))(breakOut)

      Ordering.Iterable[Int].compare(xPowers, yPowers)
    }
  }

  val ReversePurelexSimilarityOrdering = PurelexSimilarityOrdering.reverse

  implicit val DefaultExactOrdering = ReversePurelexExactOrdering

  val DefaultOrdering = ReversePurelexSimilarityOrdering

  implicit object DebugShow extends Show[Monomial] {
    override def show(m: Monomial): Cord = {
      if (m.c == Rational.zero)
        Cord("0")
      else {
        val coeffCord =
          if (m.powers.isEmpty) {
            Cord(m.c.toString)
          } else {
            if (m.c == Rational.one) {
              Cord()
            } else {
              Cord(m.c.toString, "*")
            }
          }

        val powersCord = Cord.mkCord("*", m.powers.map({
          case (v, p) =>
            if (p == 1)
              Cord(v)
            else
              Cord(v, "^", p.toString)
        }).toSeq: _*)
        coeffCord ++ powersCord
      }
    }
  }

}
