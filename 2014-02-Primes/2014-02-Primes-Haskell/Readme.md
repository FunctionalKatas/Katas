Comments
========

The first solution, in Prime0.hs, is purely functional, but is
less efficient. It sieves out multiples from a list of candidate
numbers to produce an (infinite) list of prime numbers.

The second, in Prime1.hs uses state arrays to compute the answer, so there is much more plumbing involved but performance is much better. This is the bug fixed version (I was not truncating the list of multiples early enough so I ended up calculating array indices large enough to overflow machine INT's).

The last solutions is built on the knowledge of prime numbers and, while being purely functional and declarative, it the fastest.

For the record:

        $ time stack runghc Prime0.hs
        [100519,100523,100537,100547,100549]
        stack runghc Prime0.hs  22.33s user 0.08s system 100% cpu 22.372 total
        $ time stack runghc Prime1.hs
        [100519,100523,100537,100547,100549]
        stack runghc Prime1.hs  0.72s user 0.04s system 102% cpu 0.739 total
        $ time stack runghc Prime2.hs
        [100519,100523,100537,100547,100549]
        stack runghc Prime2.hs  0.48s user 0.03s system 102% cpu 0.499 total
