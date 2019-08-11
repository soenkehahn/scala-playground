import scala.math._
import util.control.Breaks._

class Point(var x: Double, var y: Double) {
  def length(): Double = sqrt(scala.math.pow(x, 2) + scala.math.pow(y, 2))

  override def toString(): String = s"(x = $x, y = $y)"
}

abstract sealed class List[+A] extends Countable with Foreach[A] with Iterable[A] {
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

  def map_[B](f: A => B): List[B] = {
    this match {
      case Nil => Nil
      case Cons(head, tail) => Cons(f(head), tail.map_(f))
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

  def foldLeft_[B](fNil: B)(fCons: (A, B) => B): B = this match {
    case Nil => fNil
    case Cons(head, tail) => fCons(head, tail.foldLeft_(fNil)(fCons))
  }

  def size_(): Int = this match {
    case Nil => 0
    case Cons(_, tail) => 1 + tail.size_
  }

  override def foreach_(f: A => Unit): Unit = {
    var current = this
    while (current != Nil) {
      val Cons(head, tail) = current.asInstanceOf[Cons[A]]
      f(head)
      current = tail
    }
  }

  def iterator(): Iterator[A] = {
    class Iter(var current: List[A]) extends Iterator[A] {
      def hasNext(): Boolean = this.current match {
        case Nil => false
        case Cons(_, _) => true
      }
      def next(): A = this.current match {
        case Nil => throw new Exception("next: empty list")
        case Cons(head, tail) => {
          current = tail
          head
        }
      }
    }
    new Iter(this)
  }
}

case class Cons[A](head_ : A, tail_ : List[A]) extends List[A]

case object Nil extends List[Nothing]

trait Countable {
  def size_(): Int
}

trait Foreach[+A] {
  def foreach_(f: A => Unit): Unit
}
