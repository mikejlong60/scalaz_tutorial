import scalaz._
import Scalaz._

//Monad Transformers - It would be nice to be able to add something
//like failure handling to a monad without resorting to the
//construction of custom monads.  The standard monads in the mt1 library
//don't allow this but instead provide a set of monad transformers
//to achieve the same result.
//
//A monad transformer is similar to a regular monad. But it can't stand
//on its own. It modifies the behavior of an underlying monad.

//Here's an example using the Reader monad
def myName(step: String): Reader[String, String] = Reader { s: String => step + ", I am " + s }
def localExample: Reader[String, (String, String, String)] = for {
  a <- myName("First")
  b <- myName("Second") >=> Reader { s: String => s + "dy" }
  c <- myName("Third")
} yield (a, b, c)

localExample("fred")

type ReaderTOption[A, B] = ReaderT[Option, A, B]
object ReaderTOption extends KleisliInstances with KleisliFunctions {
  def apply[A, B](f: A => Option[B]): ReaderTOption[A, B] = kleisli(f)
}
def configure(key: String) = ReaderTOption[Map[String, String], String] { _.get(key) }

def setupConnection = for {
  host <- configure("host")
  user <- configure("user")
  password <- configure("password")
} yield (host, user, password)
val goodConfig = Map("host" -> "xxx.com", "user" -> "mike", "password" -> "pass")

type StateTReaderTOption[C, S, A] = StateT[({ type l[X] = ReaderTOption[C, X] })#l, S, A]

object StateTReaderTOption extends StateTInstances with StateTFunctions {
  def apply[C, S, A](f: S => (S, A)) = new StateT[({ type l[X] = ReaderTOption[C, X] })#l, S, A] {
    def apply(s: S) = f(s).point[({ type l[X] = ReaderTOption[C, X] })#l]
  }
  def get[C, S]: StateTReaderTOption[C, S, S] = StateTReaderTOption { s => (s, s) }
  def put[C, S](s: S): StateTReaderTOption[C, S, Unit] = StateTReaderTOption { _ => (s, ()) }
}

type Stack = List[Int]
type Config = Map[String, String]
val pop = StateTReaderTOption[Config, Stack, Int] {
  case x :: xs => (xs, x)
}

val pop2: StateTReaderTOption[Config, Stack, Int] = {
  import StateTReaderTOption.{ get, put }
  for {
    s <- get[Config, Stack]
    val (x :: xs) = s
    _ <- put(xs)
  } yield x
}

def push2(x: Int): StateTReaderTOption[Config, Stack, Unit] = {
  import StateTReaderTOption.{ get, put }
  for {
    xs <- get[Config, Stack]
    r <- put(x :: xs)
  } yield r
}

def stackManip2: StateTReaderTOption[Config, Stack, Int] = for {
  _ <- push2(3)
  a <- pop
  b <- pop
} yield b

stackManip2(List(5, 8, 2, 1))(Map())

def configure2[S](key: String) = new StateTReaderTOption[Config, S, String] {
  def apply(s: S) = ReaderTOption[Config, (S, String)] { config: Config =>
    config.get(key) map {
      (s, _)
    }
  }
}

def stackManip3: StateTReaderTOption[Config, Stack, Unit] = for {
  x <- configure2("x")
  a <- push2(x.toInt)
} yield a

stackManip3(List(5,8,2,1))(Map("y" -> "7"))