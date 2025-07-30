val matrix = Vector(
  Vector(1, 2, 3),
  Vector(4, 5, 6),
  Vector(7, 8, 9)
)

// ↘ Hauptdiagonalen (i - j)
val mainDiagonals: Map[Int, Vector[(Int, Int, Int)]] =
  (for {
    i <- matrix.indices
    j <- matrix(i).indices
  } yield (i - j, (i, j, matrix(i)(j)))).groupBy(_._1).view.mapValues(_.map(_._2).toVector).toMap

// ↙ Gegendiagonalen (i + j)
val antiDiagonals: Map[Int, Vector[(Int, Int, Int)]] =
  (for {
    i <- matrix.indices
    j <- matrix(i).indices
  } yield (i + j, (i, j, matrix(i)(j)))).groupBy(_._1).view.mapValues(_.map(_._2).toVector).toMap

mainDiagonals.foreach { case (d, elems) =>
  println(s"↘ Diagonal $d: ${elems.map(_._3)}")
}

antiDiagonals.foreach { case (d, elems) =>
  println(s"↙ Anti-Diagonal $d: ${elems.map(_._3)}")
}

val coords =
  for
    i <- matrix.indices
    j <- matrix(i).indices
  yield (i, j)

val main: Map[Int, IndexedSeq[(Int, Int)]] = coords.groupBy((i, j) => i - j)
val anti = coords.groupBy((i, j) => i + j)

main.view.mapValues(vs => vs.map((i, j) => matrix(i)(j))).toMap
anti.view.mapValues(vs => vs.map((i, j) => matrix(i)(j))).toMap

