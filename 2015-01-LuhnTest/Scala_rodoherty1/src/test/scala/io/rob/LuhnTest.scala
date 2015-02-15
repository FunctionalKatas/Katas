package io.rob

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll
import org.scalacheck.util.Pretty.Params
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, WordSpec}

/**
 * Created by rob on 11/02/15.
 */
class LuhnTest extends WordSpec with Matchers with Checkers {
  implicit override val generatorDrivenConfig = PropertyCheckConfig(maxDiscarded = 3000)

  "My Luhn Algorithm" should {
    "validate the provided credit cards" in {
      Luhn("49927398716") should be(true)
      Luhn("49927398717") should be(false)
      Luhn("1234567812345678") should be(false)
      Luhn("1234567812345670") should be(true)
    }

    /*
     * This test asks my CreditCardGenerator to generate a tonne of random CreditCard numbers and then
     * test each one in turn.
     */
    "validate my generated credit cards" in {
      check(new CreditCardGenerator)
    }
  }
}
