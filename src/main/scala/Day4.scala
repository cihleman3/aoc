import Day4.{findAllDirections, input, parse, xmas}

object Day4:
  val xmas = "XMAS"

  val input =
    """
      |MMMSXXMASM
      |MSAMXMSMSA
      |AMXSXMAAMM
      |MSAMASMSMX
      |XMASAMXAMM
      |XXAMMXXAMA
      |SMSMSASXSS
      |SAXAMASAAA
      |MAMMMXMMMM
      |MXMXAXMASX
      |MAMMMXMMMM
  """.stripMargin

  def parse(input: String): Array[Array[Char]] =
    input.split("\n").filterNot(_.isBlank).map(_.trim).map(_.toCharArray)

  def findAllOccurrences(line: String, word: String): List[(Int, Int)] =
    word.r.findAllMatchIn(line).map(m => m.start -> m.end).toList

  def findAllDirections(line: String): List[(Int, Int)] =
    findAllOccurrences(line, xmas) ++ findAllOccurrences(line, xmas.reverse)

@main
def run4() =
  val matrix: Array[Array[Char]] = parse(input)
  println(matrix.map(_.toList.mkString).toList)
  val coords =
    for
      i <- matrix.indices
      j <- matrix(i).indices
    yield (i, j)

  val diags = coords
    .groupBy((i, j) => i - j)
    .collect:
      case (k, vs) if vs.length >= xmas.length => k -> vs.map((i, j) => matrix(i)(j)).mkString
  val antidiags = coords
    .groupBy((i, j) => i + j)
    .collect:
      case (k, vs) if vs.length >= xmas.length => k -> vs.map((i, j) => matrix(i)(j)).mkString

  val across: List[String] = matrix.map(_.mkString).toList
  val down: List[String] = matrix.transpose.map(_.mkString).toList

  val allLines: List[List[String]] = List(across, down, diags.values.toList, antidiags.values.toList)

  val res: List[(Int, Int)] = for
    lines <- allLines
    line <- lines
    occ <- findAllDirections(line)
  yield occ

  println(res.mkString(","))
  println(res.length)
