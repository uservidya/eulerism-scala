package ru.smartislav.eulerism.scala

import scala.collection.mutable
import scala.collection.immutable.BitSet


object Problem5 extends App {
  val sieve = mutable.BitSet.empty
  for (i <- 20 to 2 by -1; j <- 2 until i; if i % j == 0) {
    sieve += j
  }
  val factors = BitSet.empty ++ (2 to 20) -- sieve

  println(factors)
  println(factors.map(_.toLong).reduce(_ * _))
}
