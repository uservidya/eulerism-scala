package ru.smartislav.eulerism.scala.poly

import ru.smartislav.eulerism.scala._
import scala.annotation.tailrec
import scalaz.{Cord, Show}

class Polynomial private(val monomials: Seq[Monomial]) {
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

  def isZero = monomials.isEmpty

  def nonZero = !isZero

  override def equals(that: Any): Boolean = {
    if (!that.isInstanceOf[Polynomial]) false
    else monomials == that.asInstanceOf[Polynomial].monomials
  }

  override def hashCode(): Int = monomials.hashCode()

  override def toString: String = Polynomial.DebugShow.shows(this)
}

object Polynomial {
  val zero = new Polynomial(Seq.empty)
  val one = Polynomial(Monomial.one)

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
      //      println(s"build($checked, $left)")
      left match {
        case l :: ls =>
          build(l :: checked, ls ++ checkOne(l))
        case Nil => checked
      }
    }

    def checkOne(f: Polynomial): List[Polynomial] = {
      //      println(s"checkOne($f)")
      if (ps.isEmpty) {
        //        println(s"checkOne($f) = List()")
        Nil
      } else {
        var ret: List[Polynomial] = Nil
        for (p <- ps.tails; if p.nonEmpty) {
          //          println(s"checkOne($f): $p")
          val s = (f sPoly p.head) reduceByBasis (p ++ ret)
          if (s != Polynomial.zero)
            ret ::= s
        }
        //        println(s"checkOne($f) = $ret")
        ret
      }
    }

    ps.toList match {
      case h :: t => build(h :: Nil, t)
      case Nil => Seq.empty
    }
  }
}
