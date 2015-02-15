package io.rob

/**
 * Created by rob on 11/02/15.
 */
object Luhn {

  def apply(s: String) = luhn(s)

  def reverseInput(s: String): List[Int] = s.toList.map(_.asDigit).reverse

  def getSums(xs: List[Int]): Int = {
    def convert(i: Int) = {
      if (i < 10) i
      else (i % 10) + 1
    }

    def loop(sum1: Int, sum2: Int, l: List[Int]): Int = {
      l match {
        case Nil => sum1 + sum2
        case a :: b :: rest => loop(sum1 + a, sum2 + convert(b * 2), rest)
        case a :: rest => loop (sum1 + a, sum2, rest)
      }
    }

    loop(0, 0, xs)
  }

  def luhn(in: String): Boolean = {
    val numbers: List[Int] = reverseInput(in)

    val sum = getSums(numbers)
    sum % 10 == 0
  }
}
