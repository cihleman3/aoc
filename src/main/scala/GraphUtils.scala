import scalax.collection.generic.Edge
import scalax.collection.immutable.Graph

object GraphUtils:
  def findMinimal[N, E <: Edge[N]](g: Graph[N, E]): Option[g.NodeT] =
    g.nodes.find(isMinimal(g)(_))

  // forme Looks like I reinvented Kahhn's algorithm here
  def isLinearExtension[N, E <: Edge[N]](g: Graph[N, E])(ls: List[N]): Either[String, Boolean] =
    if g.isEmpty
    then if ls.isEmpty then Right(true) else Left("Empty graph")
    else if g.isCyclic
    then Left("Graph is cyclical and has, hence, no linear extension.")
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

  def isMinimal[N, E <: Edge[N]](g: Graph[N, E])(n: g.NodeT): Boolean =
    n.inDegree == 0
