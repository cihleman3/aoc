import scalax.collection.OuterImplicits.*
import scalax.collection.edges.*
import scalax.collection.edges.labeled.WDiEdgeFactory
import scalax.collection.generic.{AnyEdge, Edge}
import scalax.collection.hyperedges.*
import scalax.collection.immutable.Graph
import scalax.collection.{OneOrMore, Several}

val g = Graph(1 ~> 2, 1 ~> 3, 3 ~> 4, 1 ~> 5, 6, 7 ~> 8)

val topSorted: List[g.NodeT] = g.topologicalSort(using _ => ()) match
  case Right(order) =>
    order.toList
  case Left(cycleNode) => throw new Exception(s"Graph has a cycle at $cycleNode")

println(topSorted)

val n = g.get(1)

n.outerNodeTraverser.toList

var g1 = Graph(1 ~> 2 % 1, 1 ~> 3 % 2, 2 ~> 3 % 3, 3 ~> 4 % 1)
g1 = g1 + 5
val n1 = g1.get(5)

val g2 = Graph(1 ~> 2, 1 ~> 3, 3 ~> 4, 1 ~> 5, 6)

g2.nodes.map(_.outerNodeTraverser.toList)

g2.isCyclic

val validOrder = List(6, 1, 3, 2, 4, 5, 7, 8)
val invalidOrder = List(1, 2, 8, 7, 4, 3, 5, 6)

def isMinimal[N, E <: Edge[N]](g: Graph[N, E])(n: g.NodeT): Boolean =
  n.inDegree == 0

def findMinimal[N, E <: Edge[N]](g: Graph[N, E]): Option[g.NodeT] =
  g.nodes.find(isMinimal(g)(_))

findMinimal(g)

def isLinearExtension[N, E <: Edge[N]](g: Graph[N, E])(ls: List[N]): Either[String, Boolean] =
  if g.isEmpty
  then if ls.isEmpty then Right(true) else Left("Empty graph")
  else if g.isCyclic then Left("Graph is cyclical and has, hence, no linear extension.")
  else if g.nodes.toSet != ls.toSet then Left("Nodes of sequence and graph are not the same.")
  else
    val res = ls.zipWithIndex
      .foldLeft[(Graph[N, E], Int)](g -> -1) { case ((g1, k), (n, i)) =>
        if k >= 0 then (g1, k)
        else if !isMinimal(g1)(g1.get(n)) then (g1, i)
        else (g1 - n, k)
      }
      ._2
    if res >= 0 then Left(s"Node ${ls(res)} at position $res is not minimal")
    else Right(true)

def isv = isLinearExtension(g)

isv(validOrder)
isv(invalidOrder)
