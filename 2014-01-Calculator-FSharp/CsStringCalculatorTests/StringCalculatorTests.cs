namespace UnitTestProject1
{
    using System;
    using Microsoft.VisualStudio.TestTools.UnitTesting;

    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void AddEmptyReturns0()
        {
            Assert.AreEqual(0, StringCalculator.Add(""));
        }

        [TestMethod]
        public void Add1and2()
        {
            Assert.AreEqual(3, StringCalculator.Add("1,2"));
        }

        [TestMethod]
        public void Add1()
        {
            Assert.AreEqual(4, StringCalculator.Add("4"));
        }

        [TestMethod]
        public void Add3numbers()
        {
            Assert.AreEqual(6, StringCalculator.Add("1,2,3"));
        }

        [TestMethod]
        public void NewLineDelimited()
        {
            Assert.AreEqual(6, StringCalculator.Add("1\n2,3"));
        }

        [TestMethod]
        public void NewLineDelimitedError()
        {
            try
            {
                StringCalculator.Add("1,\n");
                Assert.Fail("Expected ArgumentException");
            }
            catch (ArgumentException ex)
            {
                Assert.AreEqual("Missing number", ex.Message);
            }
        }

        [TestMethod]
        public void CustomDelimiter()
        {
            Assert.AreEqual(3, StringCalculator.Add("//;\n1;2"));
        }

        [TestMethod]
        public void OneNegativeNumber()
        {
            try
            {
                StringCalculator.Add("4,-1,2");
                Assert.Fail("Expected ArgumentException");
            }
            catch (ArgumentException ex)
            {
                Assert.AreEqual("negatives not allowed: -1", ex.Message);
            }
        }

        [TestMethod]
        public void MultipleNegativeNumbers()
        {
            try
            {
                StringCalculator.Add("4,-1,2,-5,-6");
                Assert.Fail("Expected ArgumentException");
            }
            catch (ArgumentException ex)
            {
                Assert.AreEqual("negatives not allowed: -1,-5,-6", ex.Message);
            }
        }

        [TestMethod]
        public void IgnoreGreaterThan1000()
        {
            Assert.AreEqual(4, StringCalculator.Add("1,1001,3"));
        }

        [TestMethod]
        public void DontIgnore1000()
        {
            Assert.AreEqual(1004, StringCalculator.Add("1,1000,3"));
        }

        [TestMethod]
        public void MultipeCharacterDelimiter()
        {
            Assert.AreEqual(7, StringCalculator.Add("//***\n2***1***4"));
        }
    }
}
