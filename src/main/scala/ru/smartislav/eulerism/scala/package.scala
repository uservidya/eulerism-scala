package ru.smartislav.eulerism

import _root_.scala.collection.generic.CanBuildFrom
import _root_.scala.collection.{mutable, BitSet}

package object scala {

  def merge[A, That[_]](a: Iterable[A], b: Iterable[A])
                       (implicit ord: Ordering[A], bf: CanBuildFrom[_, A, That[A]]): That[A] = {
    merge(a.iterator, b.iterator)(ord, bf)
  }

  def merge[A, That[_]](a: Iterator[A], b: Iterator[A])
                       (implicit ord: Ordering[A], bf: CanBuildFrom[_, A, That[A]]): That[A] = {
    mergeWith(a, b)({
      (aa, bb) => aa
    })(ord, bf)
  }

  def mergeWith[A, That[_]](a: Iterable[A], b: Iterable[A])(mergeFn: (A, A) => A)
                           (implicit ord: Ordering[A], bf: CanBuildFrom[_, A, That[A]]): That[A] = {
    mergeWith(a.iterator, b.iterator)(mergeFn)(ord, bf)
  }

  def mergeWith[A, That[_]](a: Iterator[A], b: Iterator[A])(mergeFn: (A, A) => A)
                           (implicit ord: Ordering[A], bf: CanBuildFrom[_, A, That[A]]) = {
    val ret = bf()

    val ia = a.buffered
    val ib = b.buffered

    while (ia.hasNext && ib.hasNext) {
      println(s"ia: ${ia.head}, ib: ${ib.head}")
      ord.compare(ia.head, ib.head).signum match {
        case 0 =>
          ret += mergeFn(ia.next(), ib.next())
        case -1 =>
          ret += ia.next()
        case 1 =>
          ret += ib.next()
      }
    }

    while (ia.hasNext) ret += ia.next
    while (ib.hasNext) ret += ib.next

    ret.result()
  }

  def groupRuns[X, That[_]](xs: Iterator[X])(groupFn: Seq[X] => X)(implicit eqv: Equiv[X], bf: CanBuildFrom[_, X, That[X]]): That[X] = {
    val ret = bf()
    var currentRun: List[X] = Nil
    while (xs.hasNext) {
      val x = xs.next()
      if (currentRun == Nil || eqv.equiv(currentRun.head, x)) {
        currentRun ::= x
      } else {
        ret += groupFn(currentRun)
        currentRun = List(x)
      }
    }
    if (currentRun != Nil)
      ret += groupFn(currentRun)
    ret.result()
  }

  def eratosthenesSieveUpTo(max: Int): BitSet = {
    val sieve = mutable.BitSet.empty
    for (i <- 2 to (max + 1) / 2 if !sieve(i)) {
      sieve ++= Range.inclusive(2 * i, max, i)
    }
    sieve.toImmutable
  }
}
