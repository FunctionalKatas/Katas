import re
import unittest

"""

Functional Katas, Week 1: String Calculator

Python's std lib regular expression module `re' is used to extract the integers
from the string, which gives a list of strings. This is then mapped to a list
of integers and the built-in sum function is used to add them up. If any of the
numbers are negative an exception is raised.

"""


def add(string):
  numbers = map(int, filter(lambda s: len(s) > 0,
                        re.split('-?[^-0-9]', string)
                        )
                )

  if len(filter(lambda i: i < 0, numbers)) > 0:
    raise Exception, "Negatives not allowed"

  return sum(numbers)


class TestAddFunction(unittest.TestCase):

  def test_empty(self):
    self.assertEqual(0, add(""))

  def test_simple(self):
    self.assertEqual(6, add("1,2,3"))

  def test_newline(self):
    self.assertEqual(10, add("1\n2\n3\n4"))

  def test_custom(self):
    self.assertEqual(21, add("1,2;3:4\n5,.6"))

  def test_slash(self):
    self.assertEqual(10, add("//;\n1;2;3;4"))
    self.assertNotEqual(4, add("//6,,4"))

  def test_negatives(self):
    self.assertRaises(Exception, add, "1,2,-4")


suite = unittest.TestLoader().loadTestsFromTestCase(TestAddFunction)
unittest.TextTestRunner(verbosity=2).run(suite)
