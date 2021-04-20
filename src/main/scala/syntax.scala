package syntax

import algs.factorial

extension (n: Int) {
  infix def choose(k: Int): BigInt =
    factorial(n) / (factorial(k) * factorial(n - k))
}
