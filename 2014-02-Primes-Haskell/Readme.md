Comments
========

Two solutions - the first in Prime0.hs is purely functional, but is
less efficient. It sueves out multiples from a list of candidate
numbers to produce an (infinite) list of prime numbers. On my 2009 vintage MBP it takes around a minute to find the answer in the interpreter (no optimiser), and about 3 seconds when compiled with the optimiser on.

The second, in Prime1.hs uses state arrays to compute the answer, so there is much more plumbing involved but performance is much better. This is the bug fixed version (I was not truncating the list of multiples early enough so I ended up calculating array indices large enough to overflow machine INT's). On the same machine this takes about a fifth of a second to run when compiled, or about a second in the interpreter.