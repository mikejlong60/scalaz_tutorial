import scalaz.Functor
import scalaz.syntax.Ops
import scalaz.Scalaz._

trait Functor[F[_]] { self =>
  /** Lift 'f' into 'F' and apply it to 'F[A]'. */
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait FunctorOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Functor[F]

  import scalaz.Leibniz.===

  final def map[B](f: A => B): F[B] = {
    println(1)
    val result = F.map(self)(f)
    println(result)
    result
  }
}

List("1","2", "3") map { _ + 33}

(1, 2, 3) map {_ + 8}

def fred = ((x: Int) => x + 1) map {_ * 7}
fred(3)












