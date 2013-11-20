package ru.smartislav.eulerism.scala


object Problem3 extends App {
  val p = 600851475143l

  def highestFactor(n: Long): Int = {
    val upperBound = math.ceil(math.sqrt(n)).toInt
    val sieve = eratosthenesSieveUpTo(upperBound)

    for (possibleFactor <- Range(upperBound, 1, -1)
         if n % possibleFactor == 0
         if !sieve(possibleFactor)) {
      return possibleFactor
    }
    1
  }

  println(highestFactor(p))
}
