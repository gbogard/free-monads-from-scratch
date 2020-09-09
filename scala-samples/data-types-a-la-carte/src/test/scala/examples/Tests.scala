package examples

import examples.ALaCarte.{program, _}

class Tests extends munit.FunSuite {

  test("this program should evaluate to 17") {

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

    assertEquals(foldExpr(evalValOrAdd)(program), 17)
  }

  test("this program should evaluate to 23") {

    val program: ValOrAddExpr = Expr(
      R(
        Add(
          Expr(L(Val(12))),
          Expr(
            R(
              Add(
                Expr(L(Val(-4))),
                Expr(L(Val(15)))
              )
            )
          )
        )
      )
    )

    assertEquals(foldExpr(evalValOrAdd)(program), 23)
  }
}
