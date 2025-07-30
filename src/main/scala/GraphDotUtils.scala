import scalax.collection.generic.Edge
import scalax.collection.immutable.Graph
import scalax.collection.io.dot.*
import scalax.collection.io.dot.implicits.{toId, toNodeId}

object GraphDotUtils:
  def graphToDot[N, E <: Edge[N]](graph: Graph[N, E]): String =
    val dotRoot = DotRootGraph(directed = true, id = Some("G"))

    val edgeTransformer: EdgeTransformer[N, E] = edge =>
      Some(
        (
          dotRoot,
          DotEdgeStmt(
            node_1Id = edge.sources.head.toString,
            node_2Id = edge.targets.head.toString,
            attrList = List.empty,
          ),
        )
      )

    graph.toDot(dotRoot, edgeTransformer)

  // Kompakte Variante für kleine Graphen - korrigierte DOT-Syntax
  def graphToDotCompact[N, E <: Edge[N]](graph: Graph[N, E]): String =
    val dotRoot = DotRootGraph(
      directed = true,
      id = Some("CompactGraph"),
      attrList = List(
        DotAttr("size", "6.4"), // Punkt statt Komma
        DotAttr("ratio", "fill"),
        // Keine node/edge Attribute hier - die Scala Library kann das nicht richtig
      ),
    )

    val edgeTransformer: EdgeTransformer[N, E] = edge =>
      Some(
        (
          dotRoot,
          DotEdgeStmt(
            node_1Id = edge.sources.head.toString,
            node_2Id = edge.targets.head.toString,
            attrList = List(
              DotAttr("fontsize", "8")
            ),
          ),
        )
      )

    val nodeTransformer: NodeTransformer[N, E] = node =>
      Some(
        (
          dotRoot,
          DotNodeStmt(
            nodeId = node.toString,
            attrList = List(
              DotAttr("shape", "circle"),
              DotAttr("width", "0.3"),
              DotAttr("height", "0.3"),
              DotAttr("fontsize", "8"),
            ),
          ),
        )
      )

    graph.toDot(dotRoot, edgeTransformer, cNodeTransformer = Some(nodeTransformer))

  def graphToDotStyled[N, E <: Edge[N]](
      graph: Graph[N, E],
      name: String = "Graph",
      directed: Boolean = true,
  ): String =

    val dotRoot = DotRootGraph(
      directed = directed,
      id = Some(s"\"$name\""),
      attrList = List(
        DotAttr("rankdir", "TB"), // Top-to-Bottom Layout
        DotAttr("bgcolor", "white"), // Hintergrundfarbe
        DotAttr("fontname", "Arial"), // Schriftart
        DotAttr("fontsize", "14"), // Schriftgröße
      ),
    )

    val edgeTransformer: EdgeTransformer[N, E] = edge =>
      Some(
        (
          dotRoot,
          DotEdgeStmt(
            node_1Id = edge.sources.head.toString,
            node_2Id = edge.targets.head.toString,
            attrList = List(
              DotAttr("color", "blue"), // Kantenfarbe
              DotAttr("fontsize", "10"), // Beschriftungsgröße
              DotAttr("arrowhead", "vee"), // Pfeilart
            ),
          ),
        )
      )

    val nodeTransformer: NodeTransformer[N, E] = node =>
      Some(
        (
          dotRoot,
          DotNodeStmt(
            nodeId = node.toString,
            attrList = List(
              DotAttr("shape", "ellipse"), // Knotenform
              DotAttr("style", "filled"), // Gefüllte Knoten
              DotAttr("fillcolor", "lightblue"), // Füllfarbe
              DotAttr("fontname", "Arial"), // Schriftart
              DotAttr("fontsize", "12"), // Schriftgröße
            ),
          ),
        )
      )

    graph.toDot(dotRoot, edgeTransformer, cNodeTransformer = Some(nodeTransformer))

// Variante mit verschiedenen Farben basierend auf Knotentyp
  def graphToDotColored[N, E <: Edge[N]](
      graph: Graph[N, E],
      name: String = "ColoredGraph",
  ): String =

    val dotRoot = DotRootGraph(
      directed = true,
      id = Some(name),
      attrList = List(
        DotAttr("rankdir", "LR"), // Left-to-Right Layout
        DotAttr("bgcolor", "white"),
      ),
    )

    val edgeTransformer: EdgeTransformer[N, E] = edge =>
      Some(
        (
          dotRoot,
          DotEdgeStmt(
            node_1Id = edge.sources.head.toString,
            node_2Id = edge.targets.head.toString,
            attrList = List(
              DotAttr("color", "darkgreen"),
              DotAttr("penwidth", "2"),
            ),
          ),
        )
      )

    val nodeTransformer: NodeTransformer[N, E] = node =>
      // Farbe basierend auf dem Hash des Knotens
      val colors = Array("lightcoral", "lightyellow", "lightgreen", "lightpink", "lightcyan")
      val color = colors(math.abs(node.hashCode()) % colors.length)

      Some(
        (
          dotRoot,
          DotNodeStmt(
            nodeId = node.toString,
            attrList = List(
              DotAttr("shape", "box"),
              DotAttr("style", "filled,rounded"),
              DotAttr("fillcolor", color),
              DotAttr("fontname", "Courier"),
              DotAttr("fontsize", "11"),
            ),
          ),
        )
      )

    graph.toDot(dotRoot, edgeTransformer, cNodeTransformer = Some(nodeTransformer))
