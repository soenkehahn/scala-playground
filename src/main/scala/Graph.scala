import scalax.collection.Graph
import scalax.collection.GraphEdge._
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue

object G {
  def shortestPath(graph: Graph[Int, UnDiEdge], from: Int, to: Int): Int = {
    var lengths = HashMap.empty[Int, Int]
    lengths += (from -> 0)
    var q: Queue[Int] = Queue()
    q += from
    while (!q.isEmpty) {
      val id = q.dequeue
      val node = graph.get(id)
      val Some(length: Int) = lengths.get(id)
      for (child <- node.diSuccessors) {
        val childLength = lengths.get(child)
        childLength match {
          case None => {
            lengths += (child.toOuter -> (length + 1))
            q += child
          }
          case Some(childLength) => {
            if (childLength > length + 1) {
              lengths += (child.toOuter -> (length + 1))
              q += child
            }
          }
        }
      }
    }
    lengths.get(to).get
  }
}
