import GraphUtils.isLinearExtension
import generators.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scalax.collection.OuterImplicits.anyToNode
import scalax.collection.edges.{DiEdge, DiEdgeImplicits}
import scalax.collection.immutable.Graph

class GraphSuite extends ScalaCheckSuite:

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1_000)

  property("cyclic graphs have no linear extensions"):
    forAll(cyclicGraphGen): g =>
      val nodes = g.nodes.map(_.outer).toList
      assert(isLinearExtension(g)(nodes).isLeft, s"Cyclic graph should reject any ordering: $g")

  test("empty graph accepts empty list"):
    val emptyGraph = Graph.empty[Int, DiEdge[Int]]
    assert(isLinearExtension(emptyGraph)(List.empty).isRight)

  property("single node graph"):
    forAll(smallInt): n =>
      val singleNodeGraph = Graph(n)
      assert(isLinearExtension(singleNodeGraph)(List(n)).isRight, s"Single node $n should accept [n]")
      assert(isLinearExtension(singleNodeGraph)(List.empty).isLeft, s"Single node $n should reject empty list")
      assert(isLinearExtension(singleNodeGraph)(List(n, n)).isLeft, s"Single node $n should reject duplicates")

  property("wrong node set returns false"):
    forAll(acyclicGraphGen, Gen.listOf(smallInt)): (g, wrongNodes) =>
      val correctNodes = g.nodes.map(_.outer).toSet
      val wrongNodeSet = wrongNodes.toSet

      if wrongNodeSet != correctNodes then
        assert(
          isLinearExtension(g)(wrongNodes).isLeft,
          s"Wrong node set should be rejected. Graph: $correctNodes, Given: $wrongNodeSet",
        )

  property("valid topological orderings return true"):
    forAll(acyclicGraphGen): g =>
      if !g.isCyclic && !g.isEmpty then
        forAll(topologicalOrderGen(g)): ordering =>
          if ordering.nonEmpty then
            assert(
              isLinearExtension(g)(ordering).isRight,
              s"Valid topological ordering should be accepted. Graph: $g, Ordering: $ordering",
            )

  property("reversed ordering is usually invalid"):
    forAll(acyclicGraphGen): g =>
      if !g.isCyclic && !g.isEmpty then
        forAll(topologicalOrderGen(g)): ordering =>
          val reversed = ordering.reverse
          if ordering.length > 2 && ordering != reversed then
            assert(
              isLinearExtension(g)(reversed).isLeft,
              s"Reversed ordering should usually be invalid. Original: $ordering, Reversed: $reversed",
            )

  property("incomplete orderings are invalid"):
    forAll(acyclicGraphGen): g =>
      if !g.isCyclic && g.nodes.nonEmpty then
        forAll(topologicalOrderGen(g), Gen.choose(0, Math.max(0, g.nodes.size - 1))): (ordering, removeIndex) =>
          if ordering.length > 1 && removeIndex < ordering.length then
            val incomplete = ordering.patch(removeIndex, Nil, 1)
            assert(
              isLinearExtension(g)(incomplete).isLeft,
              s"Incomplete ordering should be invalid. Complete: $ordering, Incomplete: $incomplete",
            )

  property("duplicate nodes make ordering invalid"):
    forAll(acyclicGraphGen): g =>
      if !g.isCyclic && !g.isEmpty then
        forAll(topologicalOrderGen(g), Gen.choose(0, Math.max(0, g.nodes.size - 1))): (ordering, dupIndex) =>
          if ordering.nonEmpty && dupIndex < ordering.length then
            val withDuplicate = ordering(dupIndex) :: ordering
            assert(
              isLinearExtension(g)(withDuplicate).isLeft,
              s"Ordering with duplicates should be invalid. Original: $ordering, With duplicate: $withDuplicate",
            )

  property("chain graphs have unique valid ordering"):
    forAll(Gen.choose(2, 6)): n =>
      val nodes = (1 to n).toList
      val edges = nodes.zip(nodes.tail).map { case (a, b) => a ~> b }
      val chainGraph = Graph.from(nodes, edges)

      assert(isLinearExtension(chainGraph)(nodes).isRight, s"Chain should accept natural ordering: $nodes")
      assert(
        isLinearExtension(chainGraph)(nodes.reverse).isLeft,
        s"Chain should reject reverse ordering: ${nodes.reverse}",
      )

  property("star graphs accept multiple valid orderings"):
    forAll(Gen.choose(2, 5)): n =>
      val root = 1
      val leaves = (2 to n).toList
      val edges = leaves.map(leaf => root ~> leaf)
      val starGraph = Graph.from(root :: leaves, edges)

      // Root must be first, but leaves can be in any order
      val validOrdering1 = root :: leaves
      val validOrdering2 = root :: leaves.reverse

      assert(
        isLinearExtension(starGraph)(validOrdering1).isRight,
        s"Star graph should accept root-first ordering: $validOrdering1",
      )
      assert(
        isLinearExtension(starGraph)(validOrdering2).isRight,
        s"Star graph should accept root-first with shuffled leaves: $validOrdering2",
      )
