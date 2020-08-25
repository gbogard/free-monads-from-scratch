package examples

case class Expr[F[_]](in: F[Expr[F]])
