object Primes extends App {

  /* Prime generator - not my own work! */
  val primes = 2 #:: sieve(3)

  def sieve(n: Int): Stream[Int] =
    if (primes.takeWhile(p => p * p <= n).exists(n % _ == 0)) sieve(n + 2)
    else n #:: sieve(n + 2)

  def findBest(maxPrimeSize: Int): (Int, Int) = {

    def findBest_(allPrimes: List[Int], considering: List[Int], bestSoFar: (Int, Int) = (0, 0)): (Int, Int) = {
      val b = considering.scanLeft(0)((a: Int, b: Int) => a + b).
        zipWithIndex.
        filter(idxdEl => allPrimes.contains(idxdEl._1))

      val best = if (b.isEmpty) bestSoFar else b.maxBy(p => p._2)
      val newBestSoFar = if (bestSoFar._2 > best._2) bestSoFar else best
      if (considering.length > newBestSoFar._2) findBest_(allPrimes, considering.tail, newBestSoFar)
      else newBestSoFar
    }

    val primesBelowLimit = primes.takeWhile(_ < maxPrimeSize).toList
    val upperBoundOfTerms = primes.scanLeft(0)((a: Int, b: Int) => a + b).takeWhile(_ < maxPrimeSize).toList.size // is this a safe assumption?  
    findBest_(primesBelowLimit, primesBelowLimit.take(upperBoundOfTerms))

  }

  println(findBest(100))
  println(findBest(1000))
  println(findBest(1000000))

}