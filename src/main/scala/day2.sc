import scalaz.syntax.Ops
import scalaz._
import Scalaz._

trait Functor[F[_]] { self =>
  /** Lift 'f' into 'F' and apply it to 'F[A]'. */
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait FunctorOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Functor[F]

  import scalaz.Leibniz.===

  final def map[B](f: A => B): F[B] = F.map(self)(f)
}

List(1, 2, 3) map { _ + 1}

(1, 2, 3) map {_ + 1}


