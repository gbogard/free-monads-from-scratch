package examples

import cats.effect.IO
import cats.implicits._
import cats.{Functor, Monad}

object ALaCarte {

  case class UserId(id: String)
  case class SubId(id: String)
  case class User(id: UserId)
  case class Subscription(id: SubId)

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

  /* Now that we have shown how functors can be combined using coproducts
  we can generalize this approach to free monads
   */

  sealed trait Free[F[_], T]
  case class Pure[F[_], T](value: T) extends Free[F, T]
  case class Impure[F[_], T](value: F[Free[F, T]]) extends Free[F, T]

  // When f is Functor, Free f is a monad, as illustrated by these instances

  implicit def freeFunctor[F[_]: Functor]: Functor[Free[F, *]] =
    new Functor[Free[F, *]] {
      override def map[A, B](fa: Free[F, A])(f: A => B): Free[F, B] = fa match {
        case Pure(x)   => Pure(f(x))
        case Impure(x) => Impure(x.map(_.map(f)))
      }
    }

  implicit def freeMonad[F[_]: Functor]: Monad[Free[F, *]] =
    new Monad[Free[F, *]] {

      override def map[A, B](fa: Free[F, A])(f: A => B): Free[F, B] =
        freeFunctor[F].fmap(fa)(f)

      override def flatMap[A, B](
          fa: Free[F, A]
      )(f: A => Free[F, B]): Free[F, B] = fa match {
        case Pure(x)   => f(x)
        case Impure(x) => Impure(x.map(_.flatMap(f)))
      }

      // This implementation is not stack safe and shouldn't be used in prod.
      override def tailRecM[A, B](
          a: A
      )(f: A => Free[F, Either[A, B]]): Free[F, B] = flatMap(f(a)) {
        case Right(x)   => Pure(x)
        case Left(next) => tailRecM(next)(f)
      }

      override def pure[A](x: A): Free[F, A] = Pure(x)
    }

  // Then we can define a simple dsl
  sealed trait UserStoreDsl[Next]
  case class GetUser[Next](id: UserId, next: User => Next)
      extends UserStoreDsl[Next]
  case class Subscribe[Next](user: User, next: Next) extends UserStoreDsl[Next]

  type UserStore[A] = Free[UserStoreDsl, A]

  implicit val userStoreDslFunctor: Functor[UserStoreDsl] =
    new Functor[UserStoreDsl] {
      override def map[A, B](fa: UserStoreDsl[A])(f: A => B): UserStoreDsl[B] =
        fa match {
          case GetUser(id, next)     => GetUser(id, user => f(next(user)))
          case Subscribe(user, next) => Subscribe(user, f(next))
        }
    }

  // Along with some smart constructors
  def getUser(id: UserId): UserStore[User] =
    Impure(GetUser(id, Pure(_)))
  def subscribe(user: User): UserStore[Unit] = Impure(Subscribe(user, Pure(())))

  // And build a program using this DSL
  val freeProgram: UserStore[User] = for {
    user <- getUser(UserId("123"))
    _ <- subscribe(user)
  } yield user

  // Finally, we define a fold over Free[F, A] that turns it into IO[A]
  trait Exec[F[_]] {
    def exec[A](fa: F[A]): IO[A]
  }

  def execAlgebra[F[_]: Functor, A](
      fa: Free[F, A]
  )(implicit exec: Exec[F]): IO[A] = fa match {
    case Pure(x)   => IO.pure(x)
    case Impure(x) => exec.exec(x).flatMap(execAlgebra[F, A](_))
  }

  implicit val execUserStore: Exec[UserStoreDsl] = new Exec[UserStoreDsl] {
    override def exec[A](fa: UserStoreDsl[A]): IO[A] = fa match {
      case GetUser(id, next) => IO(User(id)).map(next)
      case Subscribe(user, next) =>
        IO(println(s"User $user has subscribed!")).as(next)
    }
  }

  execAlgebra(freeProgram).unsafeRunSync()

}
