package miniKyo

import munit.FunSuite

class AbortTests extends FunSuite:

  test("Abort.run handles typed errors"):
    def divide(a: Int, b: Int): Int < Abort[String] =
      if b == 0 then Abort.fail("Division by zero") else a / b

    val success = Abort.run(divide(10, 2)).eval
    val failure = Abort.run(divide(10, 0)).eval

    assertEquals(success, Result.Success(5))
    assert(failure.isError)

  test("Abort.panic handles Throwable"):
    val program: String < Abort[Nothing] =
      Abort.panic(RuntimeException("error"))

    val result = Abort.run(program).eval
    result match
      case Result.Panic(t) => assertEquals(t.getMessage, "error")
      case _               => fail("Expected Panic")

  test("izumi Tag distinguishes error types"):
    assert(Tag[Abort[String]] != Tag[Abort[Int]])
    assert(Tag[Abort[Throwable]] <:< Tag[Abort[Nothing]])

  test("Multiple Abort effects compose"):
    val program: Int < (Abort[String] & Abort[Int]) =
      Abort.fail[String]("error")

    val handled1: Result[String, Int] < Abort[Int]       = Abort.run(program)
    val handled2: Result[Int, Result[String, Int]] < Any = Abort.run(handled1)
    val result                                           = handled2.eval

    result match
      case Result.Success(Result.Failure(e)) => assertEquals(e, "error")
      case _                                 => fail("Expected nested Failure")
