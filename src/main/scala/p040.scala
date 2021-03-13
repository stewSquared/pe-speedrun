object p040 extends App {

  val digits: LazyList[Int] = LazyList.from(0).flatMap(_.toString.map(_.asDigit))

  val ans = List(1,10,100,1000,10000,100000,1000000).map(digits).product

  println(ans)
}
