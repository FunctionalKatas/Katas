# Run as: python luhn.py

# First we define a function that converts an integer to a list of its digits

def toDigits(n):
	string = str(n)
	digits = map(int,string)
	return list(digits)
	
# Then we define a helper function that reverses said digits
	
def toDigitsRev(n):
	return toDigits(n)[::-1]
	
# Now we double every even numbered digit
# (since Python lists are 0-indexed it translates to every odd element here)
	
def doubleSecond(digits):
	return [2*digits[i] if i % 2 != 0 else digits[i]
							for i in range(len(digits))]

# This function ensures that any two-digit numbers left from the multiplications above
# are split into single digits.

def sumDigits(numbers):
	lists = list(map(toDigits, numbers) # convert each number to a list of digits
	joined = sum(lists,[]) # concatenate the lists
	return sum(concatenated)

# Finally the functions from above are combined to form a function
# that checks if a given integer is a valid credit card number
	
def isValid(n):
	return sumDigits( doubleSecond( toDigitsRev(n) ) ) % 10 == 0


if __name__ == '__main__':
	
	numbers = [49927398716,49927398717,1234567812345678,1234567812345670]
	
	print( list( map(isValid, numbers) ) )
