package ru.smartislav.eulerism.scala

object Problem3 extends App {
  val p = 600851475143l

  def highestFactor(n: Long): Long = {
    for (f <- Stream.range(math.ceil(math.sqrt(n)).toLong, 2l, -1l)) {
      if (n % f == 0)
        return f
    }
    1
  }

  println(highestFactor(p))
}
