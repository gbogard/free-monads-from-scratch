package example

import cats.free.Free
import cats.effect.IO
import cats.data.State
import cats.implicits._
import cats.~>

object FreeExample {

  case class UserId(id: String)
  case class SubId(id: String)
  case class User(id: UserId)
  case class Subscription(id: SubId)

  // We start by defining the DSL

  sealed trait UserStoreDsl[T]
  case class GetUser(id: UserId) extends UserStoreDsl[User]
  case class GetSubscription(userId: UserId)
      extends UserStoreDsl[Option[Subscription]]
  case class DeleteSubscription(id: SubId) extends UserStoreDsl[Unit]
  case class Subscribe(user: User) extends UserStoreDsl[Subscription]

  type UserStore[A] = Free[UserStoreDsl, A]

  // Then smart constructors

  def getUser(id: UserId): UserStore[User] = Free.liftF(GetUser(id))
  def getSubscription(id: UserId): UserStore[Option[Subscription]] =
    Free.liftF(GetSubscription(id))
  def deleteSubscription(id: SubId): UserStore[Unit] =
    Free.liftF(DeleteSubscription(id))
  def subscribe(user: User): UserStore[Subscription] =
    Free.liftF(Subscribe(user))

  // Then we build a program using our DSL

  def updateSubscription(userId: UserId): UserStore[Unit] =
    for {
      user <- getUser(userId)
      oldSub <- getSubscription(userId)
      _ <- oldSub match {
        case Some(sub) => deleteSubscription(sub.id)
        case None      => ().pure[UserStore]
      }
      _ <- subscribe(user)
    } yield ()

  // Then we write an interpreter for our DSL

  val userStoreCompiler: UserStoreDsl ~> IO =
    new (UserStoreDsl ~> IO) {
      def apply[A](fa: UserStoreDsl[A]) = fa match {
        case GetUser(id) =>
          // Fetch user from database
          User(id).pure[IO].map(_.asInstanceOf[A])
        case GetSubscription(UserId("123")) =>
          Subscription(SubId("1")).some.pure[IO].map(_.asInstanceOf[A])
        case GetSubscription(_)    =>
          Option.empty.pure[IO].map(_.asInstanceOf[A])
        case DeleteSubscription(_) =>
          IO.unit.map(_.asInstanceOf[A])
        case Subscribe(_) =>
          Subscription(SubId("new-sub")).pure[IO].map(_.asInstanceOf[A])
      }
    }

  // Finally, we use the interpreter to turn the program into an IO
  val program: IO[Unit] =
    updateSubscription(UserId("123"))
      .foldMap(userStoreCompiler)

  // We can create another interpreter for testing purposes
  val mockUser = User(UserId("123"))
  val mockCompiler: UserStoreDsl ~> IO = new (UserStoreDsl ~> IO) {
    override def apply[A](fa: UserStoreDsl[A]): IO[A] = fa match {
      case GetUser(UserId("123")) => mockUser.pure[IO].map(_.asInstanceOf[A])
      case other                  => userStoreCompiler(other)
    }
  }
}
