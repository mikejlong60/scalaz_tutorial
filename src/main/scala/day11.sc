import scalaz._
import Scalaz._

case class Point(x: Double, y: Double)
case class Color(r: Byte, g: Byte, b: Byte)
case class Turtle(position: Point, heading: Double, color: Color) {
  def forward(dist: Double): Turtle = copy(position = position.copy(
    x = position.x + dist * math.cos(heading), y = position.y + dist * math.sin(heading)))
}

val t = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))
t.forward(120)

//The problem with this is that if you are going to use immutable data structures(which you should)
//then you have to nest copies.
//imperative a.b.c.d.e += 1
//functional
//a.copy(
//  b = a.b.copy(
//    c = a.b.c.copy(
//      d = a.b.c.d.copy(
//        e = a.b.c.d.e += 1
//      )
//    )
//  )
//)
//That's a hassle.
//The idea of a lens is that you can get rid of unnecessary copy calls.

val turtlePosition = Lens.lensu[Turtle, Point] (
  (a, value) => a.copy(position = value), _.position
)

val pointX = Lens.lensu[Point, Double] (
  (a, value) => a.copy(x = value), _.x
)

val turtleX = turtlePosition >=> pointX

val t0 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))

turtleX.get(t0)
turtleX.set(t0, 5.0)

turtleX.mod(_ + 1.0, t0)

val incX = turtleX =>= {_ + 1.0}
incX(t0)

val turtleHeading = Lens.lensu[Turtle, Double] (
  (a, value) => a.copy(heading = value), _.heading
)

val pointY = Lens.lensu[Point, Double] (
  (a, value) => a.copy(y = value), _.y
)
val turtleY = turtlePosition >=> pointY

def forward2(distance: Double) = for {
  heading <- turtleHeading
  x <- turtleX += distance * math.cos(heading)
  y <- turtleY += distance * math.sin(heading)
} yield (x, y)

forward2(10.0)(t0)


