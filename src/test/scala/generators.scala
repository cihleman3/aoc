import GraphDotUtils.graphToDot
import org.scalacheck.Gen
import org.scalacheck.Gen.*
import scalax.collection.edges.{DiEdge, DiEdgeImplicits}
import scalax.collection.immutable.Graph

object generators:

  // Generator for small integers to keep graphs manageable
  val smallInt: Gen[Int] = Gen.choose(1, 100)

  // Generator for acyclic directed graphs
  val acyclicGraphGen: Gen[Graph[Int, DiEdge[Int]]] = for
    nodeCount <- Gen.choose(1, 8) // Keep small for performance
    nodes = (1 to nodeCount).toList
    // Generate edges that respect topological ordering (i < j for edge i ~> j)
    edges <- Gen.listOfN(
      Gen.choose(0, nodeCount * (nodeCount - 1) / 4).sample.getOrElse(0),
      for
        i <- Gen.oneOf(nodes)
        list = nodes.filter(_ > i) if list.nonEmpty
        j <- Gen.oneOf(list) // Only forward edges to ensure acyclic
      yield i ~> j,
    )
  yield Graph.from(nodes, edges.distinct)

  // Generator for cyclic graphs
  val cyclicGraphGen: Gen[Graph[Int, DiEdge[Int]]] = for
    nodeCount <- Gen.choose(2, 6)
    nodes = (1 to nodeCount).toList
    // Create a cycle plus some additional edges
    cycle = nodes.zip(nodes.tail :+ nodes.head).map { case (a, b) => a ~> b }
    additionalEdges <- Gen.listOf(
      for
        i <- Gen.oneOf(nodes)
        j <- Gen.oneOf(nodes.filterNot(_ == i))
      yield i ~> j
    )
  yield Graph.from(nodes, (cycle ++ additionalEdges).distinct)

  // Generate valid topological orderings
  def topologicalOrderGen(g: Graph[Int, DiEdge[Int]]): Gen[List[Int]] =
    if g.isCyclic || g.isEmpty then Gen.const(List.empty)
    else g.topologicalSort(using _ => ()).toOption.get.toList.map(_.outer)

  @main
  def testGen() =
    val g = acyclicGraphGen
    val ls = (1 to 10).map(_ => g.sample.get)
    val nonBare = ls.filterNot(_.edges.isEmpty)
    println(ls.mkString("\n"))
    println("nonBare")
    println(nonBare.mkString("\n"))

    val dots = nonBare.map(graphToDot)
    println("Generated DOT files:")
    dots.foreach(println)
    println("---")
