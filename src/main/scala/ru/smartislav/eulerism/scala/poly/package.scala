package ru.smartislav.eulerism.scala

import spire.math.Rational
import scalaz.{Cord, Show}
import scala.collection.SortedSet
import scala.collection.{SortedMap, breakOut}
import scala.annotation.tailrec

package object poly {

  class Monomial private(val c: Rational, val powers: SortedMap[String, Rational]) {
    override def toString: String = Monomial.DebugShow.shows(this)

    def *(factor: Rational): Monomial = Monomial(c * factor, powers)

    def /(factor: Rational): Monomial = Monomial(c / factor, powers)

    def +(other: Monomial): Monomial = {
      require(isSimilarTo(other), s"Can't add non-similar monomials $this and $other")
      Monomial(c + other.c, powers)
    }

    def -(other: Monomial): Monomial = {
      require(isSimilarTo(other), s"Can't subtract non-similar monomials $this and $other")
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

    def isDivisibleBy(other: Monomial): Boolean = {
      other.c != Rational.zero && other.powers.forall({
        case (v, p) => p <= powers.getOrElse(v, Rational.zero)
      })
    }

    def lcm(other: Monomial): Monomial = {
      val vars = powers.map(_._1) ++ other.powers.map(_._1)
      Monomial(c * other.c / c.gcd(other.c), vars.map({
        v => (v, Array(powers.getOrElse(v, Rational.zero), other.powers.getOrElse(v, Rational.zero)).max)
      })(breakOut): SortedMap[String, Rational])
    }
  }

  object Monomial {
    val zero = new Monomial(Rational.zero, SortedMap.empty)

    def apply(c: Rational, powers: SortedMap[String, Rational]): Monomial = {
      val nonZeroPowers = powers.filter(_._2 != Rational.zero)
      if (c == Rational.zero || nonZeroPowers.isEmpty) Monomial.zero
      else new Monomial(c, nonZeroPowers)
    }

    def apply(c: Rational, powers: (String, Rational)*): Monomial = Monomial(c, SortedMap(powers: _*))

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

        val commonVars: SortedSet[String] = x.powers.keySet union y.powers.keySet
        val xPowers: Seq[Rational] = (commonVars map (x.powers.getOrElse(_, Rational.zero)))(breakOut)
        val yPowers: Seq[Rational] = (commonVars map (y.powers.getOrElse(_, Rational.zero)))(breakOut)

        //        println(s"xPowers: $xPowers")
        //        println(s"yPowers: $yPowers")

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

    def isReducible(p: Polynomial): Boolean = {
      lt.isDivisibleBy(p.lt)
    }

    def reduce(p: Polynomial): Polynomial = {
      val q = lt / p.lt
      this - p * q
    }

    @tailrec
    final def reduceByBasis(basis: Seq[Polynomial]): Polynomial = {
      def step(): (Polynomial, Boolean) = {
        if (this != Polynomial.zero)
          for (p <- basis; if isReducible(p))
            return (reduce(p), true)
        (this, false)
      }
      val (reducedPoly: Polynomial, reduced: Boolean) = step()
      if (reduced)
        reducedPoly.reduceByBasis(basis)
      else
        reducedPoly
    }

    def sPoly(other: Polynomial): Polynomial = {
      val glcm = this.lt lcm other.lt
      this * (glcm / this.lt) - other * (glcm / other.lt)
    }
  }

  object Polynomial {
    val zero = new Polynomial(Seq.empty)

    def apply(ms: Monomial*): Polynomial = {
      implicit val ord = Monomial.ReverseSimilarityOrdering
      ordered(groupRuns(ms.sorted.iterator)(_.reduce(_ + _)))
    }

    def ordered(ms: Seq[Monomial]): Polynomial = {
      val nonZero = ms.filterNot(_ == Monomial.zero)
      if (nonZero.isEmpty) zero
      else new Polynomial(nonZero)
    }

    implicit object DebugShow extends Show[Polynomial] {
      override def show(p: Polynomial): Cord = Cord.mkCord(" + ", (p.monomials map Monomial.DebugShow.show).toSeq: _*)
    }

    def groebnerBasis(ps: Seq[Polynomial]): Seq[Polynomial] = gröbnerBasis(ps)

    def gröbnerBasis(ps: Seq[Polynomial]): Seq[Polynomial] = buchbergersAlgorithm(ps)

    def buchbergersAlgorithm(ps: Seq[Polynomial]): Seq[Polynomial] = {
      @tailrec
      def build(checked: List[Polynomial], left: List[Polynomial]): Seq[Polynomial] = {
        println(s"build($checked, $left)")
        left match {
          case l :: ls =>
            build(l :: checked, ls ++ checkOne(l))
          case Nil => checked
        }
      }

      def checkOne(f: Polynomial): List[Polynomial] = {
        println(s"checkOne($f)")
        if (ps.isEmpty) {
          println(s"checkOne($f) = List()")
          Nil
        } else {
          var ret: List[Polynomial] = Nil
          for (p <- ps.tails; if p.nonEmpty) {
            println(s"checkOne($f): $p")
            val s = (f sPoly p.head) reduceByBasis (p ++ ret)
            if (s != Polynomial.zero)
              ret ::= s
          }
          println(s"checkOne($f) = $ret")
          ret
        }
      }

      ps.toList match {
        case h :: t => build(h :: Nil, t)
        case Nil => Seq.empty
      }
    }
  }

}
