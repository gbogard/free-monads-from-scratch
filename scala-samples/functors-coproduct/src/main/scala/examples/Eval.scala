package examples

import cats.Functor

trait Eval[F[_], Outcome] {
  def evalAlgebra(algebra: F[Outcome]): Outcome
}

object Eval {

  def foldExpr[F[_]: Functor, Outcome](expr: F[Outcome])(implicit eval: Eval[F, Outcome]): Outcome =
    eval.evalAlgebra(expr)

  implicit def evalCoproduct[F[_], G[_], Outcome](
                                                      implicit evalF: Eval[F, Outcome],
                                                      evalG: Eval[G, Outcome]
                                                    ): Eval[Coproduct[F, G, *], Outcome] = new Eval[Coproduct[F, G, *], Outcome] {

    override def evalAlgebra(algebra: Coproduct[F, G, Outcome]): Outcome = algebra match {
      case Coproduct.L(fAlgebra) => evalF.evalAlgebra(fAlgebra)
      case Coproduct.R(gAlgebra) => evalG.evalAlgebra(gAlgebra)
    }
  }
}
