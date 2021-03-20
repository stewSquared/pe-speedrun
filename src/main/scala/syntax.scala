package object syntax {

  implicit class IntExtensions(n: Int) {
    import algs.factorial

    def choose(k: Int): BigInt =
      factorial(n) / (factorial(k) * factorial(n - k))
  }
}
