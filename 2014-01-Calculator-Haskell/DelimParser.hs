
-- span will split a list according to a predicate
-- We want to take the span up to a newline (say) and
-- then drop the newline from the second portion
-- of the spanned list
span1 p l = (a,tail b) where (a,b) = span p l

-- Parse a single delimeter yielding
-- the delimeter itself and the unconsumed portion of the input
find1Delim :: String -> (String,String)
find1Delim ( '[':ds ) = span1 (/= ']') ds
find1Delim (d:ds) = ([d],ds)

-- Find all delimeters in the input string
-- (it is assumed that the input starts with the
--  first character of the delimeter, or with a
--  newline indicating there are no more to be found)
-- Yields all the delimeters and the trailing portion
-- of the string (which will contain the sequence to be summed)
findDelims :: String -> ( [String] , String )
findDelims ('\n':rest) = ( [], rest )
findDelims str = ( delim:delims, rest )
           where (delim, str') = find1Delim str
                 (delims,rest) = findDelims str'

-- Calculate the delimeters to be used for a particular
-- sequence. Gives a list of the deimeters and the rest of
-- the input (which is the sequence to be summed)                 
delim :: String -> ([String],String)
delim ('/':'/':str) = findDelims str
delim str = ( [ ",", "\n" ] , str )


-- simple tests
t0 = delim ""
t1 = delim "//;\n1;2"
t2 = delim "//[***][!!!]\n1***2!!!3"



