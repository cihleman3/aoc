object Day2:
  def parse(input: String): List[List[Int]] =
    input.strip().split("\r\n").toList.map(_.split(" ").toList).map: l =>
      l.map: s =>
        s.strip().toInt

  def increasing(ls: List[Int]) =
    ls.sliding(2).forall:
      case List(l, r) => r - l >= 1 && r - l <= 3
      case _ => true

  def p(ls: List[Int]) = increasing(ls) || increasing(ls.reverse)

  @main
  def run2() =
    val input =
      """
        |7 6 4 2 1
        |1 2 7 8 9
        |9 7 6 2 1
        |1 3 2 4 5
        |8 6 4 4 1
        |1 3 6 7 9
    """.stripMargin
    val s = parse(input)
    val t = s.count(p)
    println(t)
