class FooTest extends org.scalatest.FunSuite {
  test("accessing fields") {
    assert(new Point(3, 4).x === 3)
  }

  test("Point.length") {
    val point = new Point(3, 4)
    assert(point.length === 5)
  }

  test("Point.toString") {
    val point = new Point(3, 4)
    assert(s"point = $point" === "point = (x = 3.0, y = 4.0)")
  }

  test("List.toString") {
    val list = Cons(1, Cons(2, Cons(3, Nil)))
    assert(list.toString === "[1, 2, 3, ]")
  }

  test("List.map") {
    val list = Cons(1, Cons(2, Cons(3, Nil)))
    assert(list.map(x => x * 2).toString === "[2, 4, 6, ]")
  }

  test("List.headSafe") {
    assert(Cons(1, Nil).headSafe === Some(1))
    assert(Nil.headSafe === None)
  }

  test("List.shortcutEithers") {
    assert(Nil.shortcutEithers === Right(Nil))
    assert(Cons(Right(1), Nil).shortcutEithers === Right(Cons(1, Nil)))
    assert(Cons(Left("huhu"), Nil).shortcutEithers === Left("huhu"))
    assert(Cons(Right(1), Cons(Left("huhu"), Nil)).shortcutEithers === Left("huhu"))
    assert(Cons(Left("huhu"), Cons(Right(1), Nil)).shortcutEithers === Left("huhu"))
  }
}
