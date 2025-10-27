package miniKyo

import munit.FunSuite

class VarTests extends FunSuite:

  extension [A, V: Tag](v: A < Var[V])
    def checkState(initial: V, expected: V, clue: String): Unit =
      val result = Var.run(initial)(v *> Var.get[V]).eval
      assertEquals(result, expected, clue)

    def checkResult(initial: V, expected: A, clue: String): Unit =
      val result = Var.run(initial)(v).eval
      assertEquals(result, expected, clue)

  test("Var get/set/update"):
    Var.get[Int].checkResult(initial = 42, expected = 42, clue = "get returns initial")
    Var.set(100).checkState(initial = 0, expected = 100, clue = "set updates state")
    Var.update[Int](_ + 1).checkState(initial = 0, expected = 1, clue = "update modifies state")

  test("Var composition"):
    val program: Int < Var[Int] =
      for
        _   <- Var.set(10)
        _   <- Var.update[Int](_ * 2)
        res <- Var.get[Int]
      yield res

    program.checkResult(initial = 0, expected = 20, clue = "composed operations")

  test("Var with different types"):
    Var.get[String].checkResult(initial = "hello", expected = "hello", clue = "String state")
    Var.set(List(1, 2, 3)).checkResult(initial = List.empty[Int], expected = List(1, 2, 3), clue = "List state")
