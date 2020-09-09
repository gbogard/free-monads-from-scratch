package examples

import cats.implicits._
import cats.Functor

object ALaCarte {

  /*
    We start by defining a polymorphic Expr type, whose type parameter
    corresponds to the type of expressions happening in the subtree
   */
  case class Expr[F[_]](in: F[Expr[F]])

  // Then we could define a data types for expressions consisting only of integers
  // We don't actually use the type parameter because this expressions accepts no sub-expressions
  case class Val[T](value: Int)
  type ValExpr = Expr[Val]

  // We do the same for addition
  case class Add[T](a: T, b: T)
  type AddExpr = Expr[Add]

  // We can combine these types using their coproduct
  // (in Cats this is implemented as EitherK)
  sealed trait Coproduct[F[_], G[_], A]

  case class L[A, F[_], G[_]](in: F[A]) extends Coproduct[F, G, A]
  case class R[A, F[_], G[_]](in: G[A]) extends Coproduct[F, G, A]

  type ValOrAdd[T] = Coproduct[Val, Add, T]
  type ValOrAddExpr = Expr[ValOrAdd]

  // We can build programs using expressions of type `ValOrAdd`

  // 10 + 2 + 5
  val program: ValOrAddExpr = Expr(
    R(
      Add(
        Expr(L(Val(10))),
        Expr(
          R(
            Add(
              Expr(L(Val(2))),
              Expr(L(Val(5)))
            )
          )
        )
      )
    )
  )

  /*
    To evaluate our program, we must observe that not only Val and Add are functors, but also that
    the coproduct of 2 functors is itself a functor. This allows us to fold over values of type ValOrdExpr
    to get a value.
   */
  implicit val valFunctor: Functor[Val] = new Functor[Val] {
    def map[A, B](fa: Val[A])(f: A => B): Val[B] = Val(fa.value)
  }

  implicit val addFunctor: Functor[Add] = new Functor[Add] {
    override def map[A, B](fa: Add[A])(f: A => B): Add[B] =
      Add(f(fa.a), f(fa.b))
  }

  implicit def coproductFunctor[F[_], G[_]](
      implicit functorF: Functor[F],
      functorG: Functor[G]
  ): Functor[Coproduct[F, G, *]] = new Functor[Coproduct[F, G, *]] {
    def map[A, B](fa: Coproduct[F, G, A])(f: A => B): Coproduct[F, G, B] =
      fa match {
        case L(expr) => L[B, F, G](functorF.fmap(expr)(f))
        case R(expr) => R[B, F, G](functorG.fmap(expr)(f))
      }
  }

  // Given a functor F[A] and a function F[A], we can fold over F[A] to get a value like so
  def evalVal[T](term: Val[T]): Int = term.value
  def evalAdd(term: Add[Int]): Int = term.a + term.b
  def evalValOrAdd(term: ValOrAdd[Int]) = term match {
    case L(term) => evalVal(term)
    case R(term) => evalAdd(term)
  }

  def foldExpr[F[_]: Functor, T](eval: F[T] => T)(expr: Expr[F]): T =
    eval(expr.in.map(foldExpr(eval)))

  val result: Int = foldExpr(evalValOrAdd)(program)

  /*
    We could simplify this by defining an Eval type class,
    with instances for any coproduct whose functors also have an Eval instance
 */
}
