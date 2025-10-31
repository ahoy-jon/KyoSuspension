package miniKyo

import munit.FunSuite

class IsolateTests extends FunSuite:

  test("VarIsolate.lastUpdate propagates final state"):
    val program: Int < Var[Int] =
      for
        _   <- Var.set(100)
        _   <- Var.update[Int](_ * 2)
        res <- Var.get[Int]
      yield res

    // Does nothing, it's only intended in the context of Async / Choice / ...
    val isolated = Var.isolate.lastUpdate[Int].run(program)
    val result   = Var.run(0)(isolated).eval
    assertEquals(result, 200)

  test("VarIsolate.discard prevents state leakage"):
    val program: Int < Var[Int] =
      for
        _ <- Var.set(999)
        r <- Var.get[Int]
      yield r

    val oups: Int < Var[Int] = Var.isolate.discard[Int].run(program)

    val discardedAndNext: (discarded: Int, next: Int) < Var[Int] =
      Var.set(42) *> oups.map: local =>
        Var
          .get[Int]
          .map: next =>
            (discarded = local, next = next)

    val result = Var.run(0)(discardedAndNext).eval
    assertEquals(result, (discarded = 999, next = 42)) // State remains 42, not 999

  // Test utils

  test("ConditionalUpdate: success updates Var"):

    type StateAndAbort = Var[Int] & Abort[String]

    val isolate: Isolate[StateAndAbort, Any, StateAndAbort] = Var.isolate.conditionalUpdate[String, Int](discardVarUpdatesOnFailure = {
      case "NO_PROBLEM" => false
      case _            => true
    })

    // BEGIN TEST TOOLS

    case object Unset

    type Unset = Unset.type

    def expected(initialState: Int = 0, lastState: Int | Unset = Unset, result: Result[String, Int] | Unset = Unset)(
        program: Int < StateAndAbort
    ): Unit =

      val handled: (Int, Result[String, Int]) < Any = Var.run(initialState):
        Abort
          .run(isolate.run(program))
          .map: result =>
            Var
              .get[Int]
              .map: lastState =>
                (lastState, result)

      val res = handled.eval

      lastState match
        case i: Int => assertEquals(res._1, i)
        case _      =>

      result match
        case r: Result[String, Int] => assertEquals(res._2, r)
        case _                      =>

    // END TEST TOOLS

    expected(lastState = 100):
      for
        _ <- Var.set(100)
        r <- Var.get[Int]
      yield r

    expected(lastState = 0):
      for
        _ <- Var.set(100)
        _ <- Abort.fail("YOLO")
      yield 1

    expected(initialState = 1, lastState = 1):
      for
        _ <- Var.set(100)
        _ <- Abort.fail("YOLO")
      yield 1

    expected(initialState = 10, lastState = 15, result = Result.failure("NO_PROBLEM")):
      for
        _ <- Var.update[Int](_ + 5)
        _ <- Abort.fail("NO_PROBLEM")
        _ <- Abort.fail("YOLO")
      yield 1

    expected(initialState = 10, lastState = 15, result = Result.success(0)):
      for
        _ <- Var.update[Int](_ + 5)
        _ <- Abort.run(Abort.fail("YOLO")) // the failure is handled before the "isolate"
      yield 0

end IsolateTests
