import scala.math._
import util.control.Breaks._

class Point(var x: Double, var y: Double) {
  def length(): Double = sqrt(scala.math.pow(x, 2) + scala.math.pow(y, 2))

  override def toString(): String = s"(x = $x, y = $y)"
}

abstract sealed class List[+A] extends Countable {
  override def toString(): String = {
    var result = "["
    var current = this;
    breakable {
      while (true) {
        current match {
          case Nil => break
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
      case Nil => Nil
      case Cons(head, tail) => Cons(f(head), tail.map(f))
    }
  }

  def headSafe(): Option[A] =
    this match {
      case Nil => None
      case Cons(head, _) => Some(head)
    }

  def filterSome[B](implicit ev: A <:< Option[B]): List[B] = this match {
    case Nil => Nil
    case Cons(head, tail) => ev(head) match {
      case None => tail.filterSome
      case Some(e) => Cons(e, tail.filterSome)
    }
  }

  def shortcutEithers[Error, Element](implicit ev: A <:< Either[Error, Element]): Either[Error, List[Element]] =
    this match {
      case Nil => Right(Nil)
      case Cons(head, tail) => ev(head) match {
        case Left(error) => Left(error)
        case Right(element) => tail.shortcutEithers match {
          case Left(error) => Left(error)
          case Right(tail) => Right(Cons(element, tail))
        }
      }
    }

  def foldLeft[B](fNil: B)(fCons: (A, B) => B): B = this match {
    case Nil => fNil
    case Cons(head, tail) => fCons(head, tail.foldLeft(fNil)(fCons))
  }

  override def size(): Int = this match {
    case Nil => 0
    case Cons(_, tail) => 1 + tail.size
  }
}

case class Cons[A](head: A, tail: List[A]) extends List[A]

case object Nil extends List[Nothing]

trait Countable {
  def size(): Int
}
