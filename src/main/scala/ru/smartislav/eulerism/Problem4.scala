package ru.smartislav.eulerism

object Problem4 extends App {
  def is5or6DigitPalindrome(n: Int): Boolean = {
    val d6 = n / 100000
    val d5 = n / 10000 % 10
    val d4 = n / 1000 % 10
    val d3 = n / 100 % 10
    val d2 = n / 10 % 10
    val d1 = n % 10

    if (d6 != 0)
      (d6 == d1) && (d5 == d2) && (d4 == d3)
    else // 10000 is the minimum
      (d5 == d1) && (d4 == d2)
  }

  val palindromes = for (i <- 100 to 999; j <- i to 999; n = i * j; if is5or6DigitPalindrome(i * j)) yield n
  println(palindromes.max)
}
