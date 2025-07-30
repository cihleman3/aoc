import scalax.collection.GraphTraversal.*
import scalax.collection.OuterImplicits.toOuterEdge
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.edges.labeled.WDiEdgeFactory
import scalax.collection.immutable.Graph

// Create a graph using the library
val graph = Graph(
  "A" ~> "B",
  "A" ~> "C",
  "B" ~> "D",
  "C" ~> "E",
  "C" ~> "F",
  "E" ~> "G",
)

// Get a starting node
val startNode = graph.get("A")

println("=== Depth-First Search ===")
// DFS traversal - collect all nodes
val dfsNodes = startNode.outerNodeTraverser.withKind(DepthFirst).toList
println(s"DFS nodes: $dfsNodes")

println("\n=== Breadth-First Search ===")
// BFS traversal - collect all nodes
val bfsNodes = startNode.outerNodeTraverser.withKind(BreadthFirst).toList
println(s"BFS nodes: $bfsNodes")

println("\n=== Custom Traversal with State ===")
def traverseWithDepth(start: graph.NodeT): List[(String, Int)] =
  var result = List.empty[(String, Int)]
  var currentLevel = List(start)
  var visited = Set.empty[graph.NodeT]
  var depth = 0

  while currentLevel.nonEmpty do
    val nextLevel = scala.collection.mutable.ListBuffer.empty[graph.NodeT]

    currentLevel.foreach { node =>
      if !visited.contains(node) then
        visited += node
        result = (node.outer.toString, depth) :: result // .toString für Sicherheit
        nextLevel ++= node.diSuccessors.diff(visited)
    }

    currentLevel = nextLevel.toList.distinct
    depth += 1

  result.reverse

val nodesWithDepth = traverseWithDepth(startNode)

println(s"Nodes with depth: $nodesWithDepth")

println("\n=== Path Finding ===")
// Find path between two nodes
val pathOpt = startNode.pathTo(graph.get("G"))
pathOpt match
  case Some(path) =>
    val pathNodes = path.nodes.map(_.outer).toList
    println(s"Path A -> G: $pathNodes")
  case None =>
    println("No path found")

println("\n=== All Paths ===")
// Find all paths to a target (up to certain length)
// Alle Pfade zu einem Zielknoten finden
def findAllPaths(start: graph.NodeT, target: graph.NodeT, maxLength: Int = 5): List[List[String]] =
  def dfs(current: graph.NodeT, path: List[String], visited: Set[graph.NodeT], remaining: Int): List[List[String]] =
    val currentPath = path :+ current.outer.toString

    if remaining <= 0 then List.empty
    else if current == target then List(currentPath)
    else
      val newVisited = visited + current
      current.diSuccessors.toList
        .filterNot(newVisited.contains)
        .flatMap(neighbor => dfs(neighbor, currentPath, newVisited, remaining - 1))

  dfs(start, List.empty, Set.empty, maxLength)

// Verwendung:
val targetNode = graph.get("G")
val allPaths = findAllPaths(startNode, targetNode)
println(s"All paths to G: $allPaths")

println("\n=== Traversal with Filtering ===")
// Traverse only nodes that match a condition
val filteredNodes = startNode.outerNodeTraverser
  .withKind(DepthFirst)
  .filter(node => !node.toString.contains("D")) // node ist bereits der String-Wert
  .toList
println(s"Filtered DFS (no D): $filteredNodes")

println("\n=== Edge Traversal ===")
// Traverse edges instead of nodes
val edges = startNode.outerEdgeTraverser.withKind(DepthFirst).toList
println(s"DFS edges: $edges")

println("\n=== Custom Visitor Pattern ===")
// Use visitor pattern for more control
var visitedNodes = List.empty[String]
var visitedEdges = List.empty[String]

startNode.innerNodeTraverser.withKind(DepthFirst).foreach { node =>
  visitedNodes = node.outer :: visitedNodes

  // Process outgoing edges
  node.outgoing.foreach { edge =>
    visitedEdges = edge.toString :: visitedEdges
  }
}

println(s"Visitor pattern nodes: ${visitedNodes.reverse}")
println(s"Visitor pattern edges: ${visitedEdges.reverse}")

println("\n=== Weighted Graph Example ===")
// Create weighted graph
val weightedGraph = Graph(
  "A" ~> "B" % 2,
  "A" ~> "C" % 3,
  "B" ~> "D" % 1,
  "C" ~> "D" % 4,
)

val weightedStart = weightedGraph.get("A")
val weightedNodes = weightedStart.outerNodeTraverser.withKind(DepthFirst).toList
println(s"Weighted graph DFS: $weightedNodes")

// Shortest path in weighted graph
val shortestPath = weightedStart.shortestPathTo(weightedGraph.get("D"))
shortestPath match
  case Some(path) =>
    val pathNodes = path.nodes.map(_.outer).toList
    val pathWeight = path.weight
    println(s"Shortest path A -> D: $pathNodes (weight: $pathWeight)")
  case None =>
    println("No path found")

println("\n=== Cycle Detection ===")
// Check for cycles
val hasCycle = graph.isCyclic
println(s"Graph has cycle: $hasCycle")

// Create graph with cycle
val cyclicGraph = Graph("A" ~> "B", "B" ~> "C", "C" ~> "A")
println(s"Cyclic graph has cycle: ${cyclicGraph.isCyclic}")

println("\n=== Component Analysis ===")
// Work with disconnected components
val disconnectedGraph = Graph(
  "A" ~> "B",
  "B" ~> "C", // Component 1
  "D" ~> "E", // Component 2
) + "F" // workaround um nicht konflike bei Importen zu kriegen

val components = disconnectedGraph.componentTraverser()
components.foreach { component =>
  val nodes = component.nodes.map(_.outer).toList
  println(s"Component: $nodes")
}

println("\n=== Advanced Traversal Options ===")
// Traversal with custom ordering and limits
val advancedTraversal = startNode.outerNodeTraverser
  .withKind(DepthFirst)
  .withDirection(Successors) // Only follow outgoing edges
  .withMaxDepth(2) // Limit depth
  .toList

println(s"Advanced traversal (depth <= 2): $advancedTraversal")

// Traversal with early termination
val earlyTermination = startNode.outerNodeTraverser
  .withKind(BreadthFirst)
  .takeWhile(_ != "E") // Stop when we reach "E"
  .toList

println(s"Early termination (until E): $earlyTermination")
