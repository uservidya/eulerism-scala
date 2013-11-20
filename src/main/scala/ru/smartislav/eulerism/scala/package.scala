package ru.smartislav.eulerism

import _root_.scala.collection.generic.CanBuildFrom
import _root_.scala.collection.{mutable, BitSet}

package object scala {

  def merge[A, That](a: Iterable[A], b: Iterable[A])
                    (implicit ord: Ordering[A], bf: CanBuildFrom[That, A, That]): That = {
    merge(a.iterator, b.iterator)(ord, bf)
  }

  def merge[A, That](a: Iterator[A], b: Iterator[A])
                    (implicit ord: Ordering[A], bf: CanBuildFrom[That, A, That]): That = {
    mergeWith(a, b)({
      (aa, bb) => aa
    })(ord, bf)
  }

  def mergeWith[A, That](a: Iterable[A], b: Iterable[A])(mergeFn: (A, A) => A)
                        (implicit ord: Ordering[A], bf: CanBuildFrom[That, A, That]): That = {
    mergeWith(a.iterator, b.iterator)(mergeFn)(ord, bf)
  }

  def mergeWith[A, That](a: Iterator[A], b: Iterator[A])(mergeFn: (A, A) => A)
                        (ord: Ordering[A])(bf: CanBuildFrom[That, A, That]): That = {
    mergeWith(a, b)(mergeFn)(ord, bf)
  }

  def mergeWith[A, That](a: Iterator[A], b: Iterator[A])(mergeFn: (A, A) => A)
                        (implicit ord: Ordering[A], bf: CanBuildFrom[That, A, That]): That = {
    val ret = bf()

    val ia = a.buffered
    val ib = b.buffered

    while (ia.hasNext && ib.hasNext) {
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

  def eratosthenesSieveUpTo(max: Int): BitSet = {
    val sieve = mutable.BitSet.empty
    for (i <- 2 to (max + 1) / 2 if !sieve(i)) {
      sieve ++= Range.inclusive(2 * i, max, i)
    }
    sieve.toImmutable
  }
}
