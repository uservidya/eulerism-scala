package ru.smartislav.eulerism.scala

import spire.math.Rational
import scalaz.{Cord, Show}
import scala.collection.immutable.SortedSet
import scala.collection.breakOut
import scala.annotation.tailrec

package object poly {

  class Monomial private(val c: Rational, val powers: Map[String, Rational]) {
    override def toString: String = Monomial.DebugShow.shows(this)

    def *(factor: Rational): Monomial = Monomial(c * factor, powers)

    def /(factor: Rational): Monomial = Monomial(c / factor, powers)

    def +(other: Monomial): Monomial = {
      require(isSimilarTo(other))
      Monomial(c + other.c, powers)
    }

    def -(other: Monomial): Monomial = {
      require(isSimilarTo(other))
      Monomial(c - other.c, powers)
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
      val vars = powers.map(_._1) ++ other.powers.map(_._1)
      Monomial(c / other.c, (vars map {
        v =>
          v -> (powers.getOrElse(v, Rational.zero) - other.powers.getOrElse(v, Rational.zero))
      }).toSeq: _*)
    }

    def isSimilarTo(other: Monomial): Boolean = powers == other.powers

    def isDivisible(other: Monomial): Boolean = other.powers.forall({
      case (v, p) => powers.getOrElse(v, Rational.zero) < p
    })
  }

  object Monomial {
    val zero = new Monomial(Rational.zero, Map.empty)

    def apply(c: Rational, powers: Map[String, Rational]): Monomial = {
      if (c == Rational.zero || powers.isEmpty) Monomial.zero
      else new Monomial(c, powers)
    }

    def apply(c: Rational, powers: (String, Rational)*): Monomial = Monomial(c, Map(powers: _*))

    object ExactOrdering extends Ordering[Monomial] {
      def compare(x: Monomial, y: Monomial): Int = {
        SimilarityOrdering.compare(x, y) match {
          case 0 => x.c compareTo y.c
          case r => r
        }
      }
    }

    implicit val ReverseExactOrdering = ExactOrdering.reverse

    object SimilarityOrdering extends Ordering[Monomial] {
      def compare(x: Monomial, y: Monomial): Int = {
        val commonVars = x.powers.keySet ++ y.powers.keySet
        val xPowers = commonVars map (x.powers.getOrElse(_, Rational.zero))
        val yPowers = commonVars map (y.powers.getOrElse(_, Rational.zero))

        Ordering.Iterable[Rational].compare(xPowers, yPowers)
      }
    }

    val ReverseSimilarityOrdering = SimilarityOrdering.reverse

    implicit object DebugShow extends Show[Monomial] {
      override def show(m: Monomial): Cord = {
        if (m.c == Rational.zero)
          Cord.empty
        else {
          val coeffCord = if (m.c == Rational.one) Cord.empty else Cord(m.c.toString, "*")
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

  class Polynomial private(val monomials: Seq[Monomial]) {
    override def toString: String = Polynomial.DebugShow.shows(this)

    def lt = monomials.head

    def +(r: Polynomial): Polynomial = {
      implicit val ord = Monomial.ReverseSimilarityOrdering
      Polynomial(mergeWith(monomials, r.monomials)((a, b) => a + b): _*)
    }

    def -(r: Polynomial): Polynomial = {
      this + -r
    }

    def unary_-(): Polynomial = {
      Polynomial(monomials map (-_): _*)
    }

    def *(m: Monomial): Polynomial = {
      Polynomial(monomials map (_ * m): _*)
    }

    def *(p: Polynomial): Polynomial = {
      Polynomial((for (i <- monomials; j <- p.monomials) yield i * j): _*)
    }

    def /(m: Monomial): Polynomial = {
      Polynomial(monomials map (_ / m): _*)
    }

    def /(p: Polynomial): Polynomial = {
      ???
    }

    def isReducible(p: Polynomial): Boolean = {
      lt.isDivisible(p.lt)
    }

    def reduce(p: Polynomial): Polynomial = {
      val q = lt / p.lt
      val pp = p * q
      this - pp
    }

    @tailrec
    final def reduceByBasis(basis: Seq[Polynomial]): Polynomial = {
      def step(b: Seq[Polynomial], reduced: Boolean): (Polynomial, Boolean) = {
        if (this != Polynomial.zero)
          for (p <- b)
            if (isReducible(p))
              (reduce(p), true)
        (this, reduced)
      }
      val (reducedPoly: Polynomial, reduced: Boolean) = step(basis, false)
      if (reduced)
        reducedPoly.reduceByBasis(basis)
      else
        reducedPoly
    }
  }

  object Polynomial {
    val zero = new Polynomial(Seq.empty)

    def apply(ms: Monomial*): Polynomial = {
      implicit val ord = Monomial.ReverseSimilarityOrdering
      ordered(groupRuns(ms.sorted.iterator)(_.reduce(_ + _)))
    }

    def ordered(ms: Seq[Monomial]): Polynomial = {
      if (ms.isEmpty) zero
      else new Polynomial(ms)
    }

    implicit object DebugShow extends Show[Polynomial] {
      override def show(p: Polynomial): Cord = Cord.mkCord(" + ", (p.monomials map Monomial.DebugShow.show).toSeq: _*)
    }

  }

}
