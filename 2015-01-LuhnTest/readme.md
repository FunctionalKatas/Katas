### Luhn test

The Luhn test is used by some credit card companies to distinguish valid credit card numbers from what could be a random selection of digits.
Those companies using credit card numbers that can be validated by the Luhn test have numbers that pass the following test:
Reverse the order of the digits in the number.
Take the first, third, ... and every other odd digit in the reversed digits and sum them to form the partial sum s1
Taking the second, fourth ... and every other even digit in the reversed digits:
Multiply each digit by two and sum the digits if the answer is greater than nine to form partial sums for the even digits
Sum the partial sums of the even digits to form s2
If s1 + s2 ends in zero then the original number is in the form of a valid credit card number as verified by the Luhn test.
For example, if the trial number is 49927398716:

Reverse the digits:
```
>    61789372994
> Sum the odd digits:
>    6 + 7 + 9 + 7 + 9 + 4 = 42 = s1
>    The even digits:
>    1,  8,  3,  2,  9
>    Two times each even digit:
>    2, 16,  6,  4, 18
>      Sum the digits of each multiplication:
>    2,  7,  6,  4,  9
>      Sum the last:
>    2 + 7 + 6 + 4 + 9 = 28 = s2
```

>    s1 + s2 = 70 which ends in zero which means that 49927398716 passes the Luhn test


One of the answers from Phil Trelford

<blockquote class="twitter-tweet" data-partner="tweetdeck"><p><a href="https://twitter.com/fsibot">@fsibot</a> (&quot;49927398716&quot;|&gt;Seq.mapi(fun i x-&gt;i,int x-48)|&gt;Seq.fold(fun a (i,n)-&gt;match i%2,2*n with 1,n when n&gt;9-&gt;a+n-9|1,n-&gt;a+n|_-&gt;a+n)0)%10=0</p>&mdash; Philae Trelford (@ptrelford) <a href="https://twitter.com/ptrelford/status/543746917914521600">December 13, 2014</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>
