package ru.smartislav.eulerism.scala


object Problem1 extends App {
  val answer = merge(3 until 1000 by 3, 5 until 1000 by 5).sum
  println(answer)
}
