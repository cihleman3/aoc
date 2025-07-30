object Day1:

  def distance(ls1: List[Int], ls2: List[Int]): Int =
    ls1.sorted.zip(ls2.sorted).map((x, y) => Math.abs(x - y)).sum

  @main
  def run1() =
    val ls1 = List(3, 4, 2, 1, 3, 3)
    val ls2 = List(4, 3, 5, 3, 9, 3)
    val n = distance(ls1, ls2)
    println(n)


