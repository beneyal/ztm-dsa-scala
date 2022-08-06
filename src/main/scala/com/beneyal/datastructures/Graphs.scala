package com.beneyal.datastructures

object Graphs {
  final case class UndirectedGraph[A](adjList: Map[A, Set[A]]) {
    def addVertex(a: A): UndirectedGraph[A] =
      UndirectedGraph(adjList + (a -> Set()))

    def addEdge(u: A, v: A): UndirectedGraph[A] = {
      require(adjList.contains(u), s"Vertex $u was not added to the graph. Call `addVertex` first.")
      require(adjList.contains(v), s"Vertex $v was not added to the graph. Call `addVertex` first.")

      val newVs = adjList(u) + v
      val newUs = adjList(v) + u

      UndirectedGraph(adjList + (u -> newVs) + (v -> newUs))
    }

    override def toString: String = {
      adjList
        .map { case (a, bs) =>
          s"$a --> ${bs.mkString(", ")}"
        }
        .mkString("\n")
    }
  }

  object UndirectedGraph {
    def empty[A]: UndirectedGraph[A] = UndirectedGraph(Map.empty)
  }

  def main(args: Array[String]): Unit = {
    val graph = UndirectedGraph
      .empty[Char]
      .addVertex('0')
      .addVertex('1')
      .addVertex('2')
      .addVertex('3')
      .addVertex('4')
      .addVertex('5')
      .addVertex('6')
      .addEdge('3', '1')
      .addEdge('3', '4')
      .addEdge('4', '2')
      .addEdge('4', '5')
      .addEdge('1', '2')
      .addEdge('1', '0')
      .addEdge('0', '2')
      .addEdge('6', '5')
    println(graph)
  }
}
