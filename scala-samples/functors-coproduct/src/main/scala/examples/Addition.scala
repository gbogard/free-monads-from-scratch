package examples

import cats.implicits._
import cats.Functor
import Coproduct._

object Addition {

  case class Val[T](value: Int) extends AnyVal

  case class Add[T](a: T, b: T)

  type ValOrAdd = Expr[Coproduct[Val, Add, *]]

  // 10 + 2 + 5
  val additionExample: ValOrAdd = Expr(
    R(Add(
      Expr(L(Val(10))),
      Expr(R(Add(
        Expr(L(Val(2))),
        Expr(L(Val(5))),
      )))
    ))
  )

  implicit val valFunctor: Functor[Val] = new Functor[Val] {
    override def map[A, B](fa: Val[A])(f: A => B): Val[B] = fa.asInstanceOf[Val[B]]
  }

  implicit val addFunctor: Functor[Add] = new Functor[Add] {
    override def map[A, B](fa: Add[A])(f: A => B): Add[B] = Add(f(fa.a), f(fa.b))
  }

  implicit val evalVal: Eval[Val, Int] =  { case Val(int) => int }
  implicit val evalAdd: Eval[Add, Int] = { case Add(a, b) => a + b }

}
