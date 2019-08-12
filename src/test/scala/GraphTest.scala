
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import G._

class GraphTest extends org.scalatest.FunSuite {
  test("shortestPath") {
    val graph = Graph(
      1 ~ 2, 1 ~ 3, 1 ~ 6,
      2 ~ 3, 2 ~ 4,
      3 ~ 6, 3 ~ 4,
      4 ~ 5,
      6 ~ 5,
    )
    assert(shortestPath(graph, 1, 3) === 1)
    assert(shortestPath(graph, 1, 4) === 2)
    assert(shortestPath(graph, 1, 5) === 2)
  }
}
