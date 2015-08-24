import scalaz._
import Scalaz._

//Applicative builder -- At the end of day6 the author stuck
//in the applicative builder "|@|".  This operator lets you
//work with functions that take more than one parameter. Like this:
(3.some |@| 5.some) {_ + _}

val f = ({(_: Int) * 2} |@| {(_: Int) + 10}) {_ + _}

f(10) // == 40 ====> (10 * 2) + (10 + 10)


//Haskell has the state monad. It makes functions that change state pure...
//They don't change anything in the outside world.  Its like the Reader
//monad where you get a new reader returned with your function applied to the reader
//you passed in, your own copy of the universe so to speak.
//s -> (a, s)
type Stack = List[Int]
def pop(stack: Stack): (Int, Stack) = stack match {
  case x :: xs =>  (x, xs)
}

def push(a: Int,stack: Stack): (Unit, Stack) = ((), a :: stack)

def stackManip(stack: Stack): (Int, Stack) = {
  val (_, newStack1) = push(3, stack)
  val (a, newStack2) = pop(newStack1)
  pop(newStack2)
}

stackManip(List(5,8,2,1))

//Unlike general monads Scalaz's State monad specifically wraps functions.
//Here is its definition:
//type State[S, +A] = StateT[Id, S, A]

//We can construct a new state using the State singleton:
State[List[Int], Int] {case x :: xs => (xs, x)}

val pop2 = State[Stack, Int] {
  case x :: xs => (xs, x)
}
def push2(a: Int) = State[Stack, Unit] {
  case xs => (a :: xs, ())
}
def stackManip2: State[Stack, Int] = for {
  _ <- push2(3)
  a <- pop2
  b <- pop2
} yield b

stackManip2(List(5,8,2,1))

def stackyStack: State[Stack, Unit] = for {
  stackNow <- get
  r <- if (stackNow === List(1,2,3)) put(List(8,3,1))
      else put(List(9,2,1))
  } yield r

stackyStack(List(1,2,3))
stackyStack(List(3,2,1))

//You can also implement pop2 and push2 in terms of get and put.
val pop3: State[Stack, Int] = for {
  s <- get[Stack]
  (x :: xs) = s
  _ <- put(xs)
} yield x

def push3(x: Int): State[Stack, Unit] = for {
  xs <- get[Stack]
  r <- put(x :: xs)
} yield r

def stackManip3: State[Stack, Int] = for {
  _ <- push3(3)
  a <- pop3
  b <- pop3
} yield b

stackManip3(List(5,8,2,1))

// \/ -- This type is equivalent to Scala either.  It allows you
//to have a context of possible failures and allows you to attach
//values to such failures.  Convention dictates that Left is used
//for failure and Right is used for success.  Its sort of like Option:
//Left is None and Right is Some.

1.right[String]

"error".left[Int]

//The Either type in Scala is not a monad because it does not implement
//flatMap.  You have to call right to turn it into a RightProjection.
//That is silly since the whole point of Either is to report
//an error on the left.

Left[String, Int]("fred").right flatMap {x => Right[String, Int](x + 1)}

"boom".left[Int] >>= {x => (x + 1).right}

for {
  e1 <- "event 1 ok".right
  e2 <- "event 2 failed!".left[String]
  e3 <- "event 3 failed!".left[String]
} yield e1 |+| e2 |+| e3

//The first failure rolls up as the final result. How do you get the
//value out of \/  ? Use the isRight and isLeft methods.

"event 1 ok.".right.isRight

"event 1 ok".right.isLeft

//Validation is another data structure similar to Either (\/)in Scalaz.
//However, unlike \/ Validation allows you to chain validations. Its
//not a Monad like Either is not a monad, doesn't stop on a failure.
(("event 10 ok").success[String] |@| "event 2 failed!".failure[String] |@| "event 3 ok".success[String]) {_ + _ + _}

