def sumodd(numbers):
  oddNumbers = filter(lambda x: x % 2 == 1, numbers)
  return sum(oddNumbers)

class TestClass:
  def test_sumodd_one(self):
    assert sumodd([2,3,4,6,5,7,8,0,1]) == 16
  def test_sumodd_two(self):
    assert sumodd([-50,-43,-5,0,-33,21,98,64,11]) == -49
