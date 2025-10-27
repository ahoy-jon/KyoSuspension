package miniKyo

import munit.FunSuite

class TypeMapTests extends FunSuite:

  test("create and retrieve values"):
    val tm = TypeMap("hello", 42, true)
    assertEquals(tm.get[String], "hello")
    assertEquals(tm.get[Int], 42)
    assertEquals(tm.get[Boolean], true)

  test("immutability"):
    val original = TypeMap("hello")
    val updated  = original.add(42)
    assertEquals(original.get[String], "hello")
    assertEquals(updated.get[String], "hello")
    assertEquals(updated.get[Int], 42)

  test("union with overwrite"):
    val tm1   = TypeMap("first", 42)
    val tm2   = TypeMap("second", true)
    val union = tm1.union(tm2)
    assertEquals(union.get[String], "second")
    assertEquals(union.get[Boolean], true)

  test("missing type throws"):
    val tm = TypeMap("hello")
    intercept[RuntimeException](tm.get[Int])

class TagTests extends FunSuite:

  test("Tag equality"):
    assertEquals(Tag[String], Tag[String])
    assert(Tag[String] != Tag[Int])

  test("Tag subtype checking"):
    trait Animal
    class Dog extends Animal
    assert(Tag[Dog] <:< Tag[Animal])
    assert(!(Tag[Animal] <:< Tag[Dog]))

  test("izumi Tag preserves type parameters"):
    assert(Tag[List[Int]] != Tag[List[String]])
    assert(Tag[Abort[String]] != Tag[Abort[Int]])
