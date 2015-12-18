sealed trait Toy[+A, +Next]
case class Output[A, Next](a: A, next: Next) extends Toy[A, Next]
case class Bell[Next](next: Next) extends Toy[Nothing, Next]
case class Done() extends Toy[Nothing, Nothing]

Output('A', Done())
Bell(Output('s', Done()))

sealed trait CharToy[+Next]
object CharToy {

  case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]

  case class CharBell[Next](next: Next) extends CharToy[Next]

  case class CharDone() extends CharToy[Nothing]

  def output[Next](a: Char, next: Next): CharToy[Next] = CharOutput(a, next)

  def bell[Next](next: Next): CharToy[Next] = CharBell(next)

  def done: CharToy[Nothing] = CharDone()
}

import CharToy._
output('s', Done)

bell(output('d', done))

case class Fix[F[_]](f: F[Fix[F]])
object Fix {
  def fix(toy: CharToy[Fix[CharToy]]) = Fix[CharToy](toy)

}

import Fix._
fix(output('p', fix(done)))
fix(bell(fix(output('l', fix(done)))))

//stopped at FixE