Luhn CreditCard Validator - FunctionalKats January 2015
=======================================================

The main code is in ```Luhn.scala```
The main unit test is ```LuhnTest.scala```

The unit tests use ScalaTest and ScalaCheck.

ScalaCheck (which is equivalent to Haskell's QuickCheck) is a testing framework which allows automatic generation of
inputs for your test.

My CreditCard generator randomly generates 100 valid creditcards and 100 invalid creditcards and then tests each one
in turn.
