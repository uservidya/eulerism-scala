package ru.smartislav.eulerism.scala

import spire.math.Rational
import scalaz.{Cord, Show}

package object poly {

  class Monomial(val c: Rational, val powers: Map[String, Rational]) {
    override def toString: String = Monomial.DebugShow.shows(this)

    def *(factor: Rational): Monomial = Monomial(c * factor, powers)

    def /(factor: Rational): Monomial = Monomial(c / factor, powers)

    def +(other: Monomial): Monomial = {
      require(similarTo(other))
      Monomial(c + other.c, powers)
    }

    def -(other: Monomial): Monomial = {
      require(similarTo(other))
      Monomial(c - other.c, powers)
    }

    def unary_-(): Monomial = {
      Monomial(-c, powers)
    }

    def *(other: Monomial): Monomial = {
      val vars = powers.map(_._1) ++ other.powers.map(_._1)
      Monomial(c * other.c, (vars map {
        v =>
          v -> (powers.getOrElse(v, Rational.zero) + other.powers.getOrElse(v, Rational.zero))
      }).toSeq: _*)
    }

    def similarTo(other: Monomial): Boolean = powers == other.powers
  }

  object Monomial {
    val zero = new Monomial(Rational.zero, Map.empty)

    def apply(c: Rational, powers: Map[String, Rational]): Monomial = {
      if (c == Rational.zero) Monomial.zero
      else new Monomial(c, powers)
    }

    def apply(c: Rational, powers: (String, Rational)*): Monomial = Monomial(c, Map(powers: _*))

    implicit object TotalOrdering extends Ordering[Monomial] {
      def compare(x: Monomial, y: Monomial): Int = {
        (x.c, y.c) match {
          case (Rational.zero, Rational.zero) =>
            0
          case (Rational.zero, _) =>
            -1
          case (_, Rational.zero) =>
            1
          case (xc, yc) =>
            val commonVars = x.powers.map(_._1) ++ y.powers.map(_._1)
            val xPowers = commonVars map (x.powers.getOrElse(_, Rational.zero))
            val yPowers = commonVars map (y.powers.getOrElse(_, Rational.zero))

            Ordering.Iterable[Rational].compare(xPowers, yPowers) match {
              case 0 => xc compareTo yc
              case r => r
            }
        }
      }
    }

    object SimilarityOrdering extends Ordering[Monomial] {
      def compare(x: Monomial, y: Monomial): Int = {
        val commonVars = x.powers.keySet ++ y.powers.keySet
        val xPowers = commonVars map (x.powers.getOrElse(_, Rational.zero))
        val yPowers = commonVars map (y.powers.getOrElse(_, Rational.zero))

        Ordering.Iterable[Rational].compare(xPowers, yPowers)
      }
    }

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

  case class Polynomial private(monomials: Seq[Monomial]) {
    override def toString: String = Polynomial.DebugShow.shows(this)

    def lt = monomials.head

    def +(r: Polynomial): Polynomial = {
      Polynomial(mergeWith(monomials, r.monomials)((a, b) => a + b)(Monomial.SimilarityOrdering))
    }

    def -(r: Polynomial): Polynomial = {
      Polynomial(mergeWith(monomials, r.monomials)((a, b) => a - b)(ord = Monomial.SimilarityOrdering))
    }

    //    def *(r: Polynomial): Polynomial = {
    //
    //    }
  }

  object Polynomial {
    def apply(ms: Monomial*): Polynomial = {
      ms.sorted(Monomial.SimilarityOrdering).fold()
      Polynomial(ms: _*)
    }

    implicit object DebugShow extends Show[Polynomial] {
      override def show(p: Polynomial): Cord = Cord.mkCord(" + ", (p.monomials map Monomial.DebugShow.show).toSeq: _*)
    }

  }

}
