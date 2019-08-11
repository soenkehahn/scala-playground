import scala.math._
import util.control.Breaks._

class Point(var x: Double, var y: Double) {
  def length(): Double = sqrt(scala.math.pow(x, 2) + scala.math.pow(y, 2))

  override def toString(): String = s"(x = $x, y = $y)"
}

abstract sealed class List[A] {
  override def toString(): String = {
    var result = "["
    var current = this;
    breakable {
    while (true) {
        current match {
          case Nil() => break
          case Cons(head, tail) => {
            result = result.concat(s"$head, ")
            current = tail
          }
        }
      }
    }
    result = result.concat("]")
    return result
  }

  def map[B](f: A => B): List[B] = {
    this match {
      case Nil() => Nil()
      case Cons(head, tail) => Cons(f(head), tail.map(f))
    }
  }
}

case class Cons[A](head: A, tail: List[A]) extends List[A]

case class Nil[A]() extends List[A]
