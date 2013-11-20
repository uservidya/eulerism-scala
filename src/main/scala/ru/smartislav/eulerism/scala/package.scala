package ru.smartislav.eulerism

import _root_.scala.collection.{mutable, BitSet}

package object scala {

  def merge[A: Ordering](a: Iterable[A], b: Iterable[A]): Vector[A] = merge(a.iterator, b.iterator)

  def merge[A: Ordering](a: Iterator[A], b: Iterator[A]): Vector[A] = {
    val ret = Vector.newBuilder[A]
    implicit val ordering = implicitly[Ordering[A]]

    val ia = a.buffered
    val ib = b.buffered

    while (ia.hasNext && ib.hasNext) {
      ordering.compare(ia.head, ib.head).signum match {
        case 0 =>
          ret += ia.next
          ib.next()
        case -1 =>
          ret += ia.next
        case 1 =>
          ret += ib.next
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
