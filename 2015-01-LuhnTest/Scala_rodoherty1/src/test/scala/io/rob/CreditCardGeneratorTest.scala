package io.rob

import org.scalatest.{Matchers, WordSpec}

/**
 * Created by rob on 14/02/15.
 */
class CreditCardGeneratorTest extends WordSpec with Matchers {

  "My CreditCardGenerator" should {
    "merge two lists together" in  {
      val l1 = List(1, 4, 8, 1, 9, 7)
      val l2 = List(5, 2, 7, 9, 3)
      (new CreditCardGenerator).merge(l1, l2) should equal (List(7, 3, 9, 9, 1, 7, 8, 2, 4, 5, 1))
    }
  }


}
