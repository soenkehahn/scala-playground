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
}
