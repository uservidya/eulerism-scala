package ru.smartislav.eulerism

object Problem2 extends App {
  lazy val fib: Stream[Int] = 0 #:: 1 #:: fib.zip(fib.tail).map { n => n._1 + n._2 }
  println(fib.takeWhile(_ <= 4000000).filter(_ % 2 == 0).sum)
}
