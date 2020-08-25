package examples

import cats.Functor

sealed trait Coproduct[F[_], G[_], A]

object Coproduct {

  case class L[A, F[_], G[_]](in: F[A]) extends Coproduct[F, G, A]

  case class R[A, F[_], G[_]](in: G[A]) extends Coproduct[F, G, A]

  implicit def coproductFunctor[F[_], G[_]](implicit functorF: Functor[F], functorG: Functor[G]): Functor[Coproduct[F, G, *]] = new Functor[Coproduct[F, G, *]] {
    def map[A, B](fa: Coproduct[F, G, A])(f: A => B): Coproduct[F, G, B] = fa match {
      case L(expr) => L[B, F, G](functorF.fmap(expr)(f))
      case R(expr) => R[B, F, G](functorG.fmap(expr)(f))
    }
  }
}
