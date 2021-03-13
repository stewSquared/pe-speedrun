object p038 extends App {

  def concatenatedProducts(n: Int): LazyList[String] = {
    LazyList.from(1).scanLeft("")((acc, m) => acc + (m*n).toString)
  }

  def pandigitalProduct(n: Int): Option[Int] = {
    concatenatedProducts(n)
      .dropWhile(_.length < 9)
      .takeWhile(_.length < 10)
      .collectFirst{ case s if s.sorted == "123456789" => s.toInt }
  }

  val ans = (1 to 10_000).flatMap(pandigitalProduct).max

  println(ans)
}
