# Understanding Kyo: Suspensions and Rotation

A minimal implementation exploring how Kyo's algebraic effect system works through **suspension** and **rotation**. This is an educational project to understand the mechanics behind Kyo's elegant API.

## The Problem: Effect Systems That Don't Scale

Traditional effect systems face a fundamental challenge: how do you compose multiple effects without syntactic explosion?

### The IO Monad Approach

```scala ignore
// ZIO-style: explicit wrapping everywhere (conceptual - not in this project)
val program: IO[String, Int] = 
  IO.succeed(42).flatMap(n => 
    IO.fail("error").catchAll(e =>
      IO.succeed(n + 1)))
```

Every value needs `IO.succeed`. Every operation is explicitly monadic. Adding another effect to support streaming (`Emit`) means another type parameter: `IO[R, E, A, V]`. The API becomes unwieldy. See `00_example_zioLikeEncoding.scala` for a complete example.

### The Kyo Approach

```scala
import miniKyo.*

// Kyo: direct values, effects as markers
val program: Int < Abort[String] = 42  // automatic lifting!
```

No `IO.succeed`, no explicit constructors. Effects are tracked in the type (`< Abort[String]`) but values remain direct. This is **suspension** in action.

## What Is a Suspension?

A suspension is a **hole in your program** - a computation that's been paused, waiting for someone to decide what happens next.

### Under the Hood


A simplified encoding for suspensions in Kyo:

```scala ignore
trait ArrowEffect[-Input, +Output]

sealed trait <[+A, -S]

case class Pure[A](value: A) extends (A < Any)

case class Suspend[I, O, E <: ArrowEffect[I, O], A, S](
  tag: Tag[E],
  input: I,
  continue: O => A < S
) extends (A < (E & S))
```

A `Suspend` has three parts:
1. **`tag`**: Which effect is this? (e.g., `Tag[Abort[String]]`)
2. **`input`**: What data does this suspension carry? (e.g., `"error message"`)
3. **`continue`**: What happens after? A function from `O` to the rest of the computation.

### Concrete Examples
```scala
import miniKyo.<.Pure
import miniKyo.<.Suspend
```

```scala

// When you write:
Abort.fail("Oops")

// Kyo creates:
Suspend(
  tag = Tag[Abort[String]], 
  input = Result.Failure("Oops"),
  continue = _ => ??? /* never resumes */
)
```

```scala
// When you write:
Var.update[Int](_ + 1).map(i => s"i = $i")

// Kyo creates:
Suspend(
  tag = Tag[Var[Int]],
  input = Var.Op.Update(_ + 1),
  continue = output /* : Int */ => Pure(s"i = $output")
)
```

```scala
// When you write:
Emit.value("Hello") *> Emit.value("World")

// Kyo creates nested suspensions:
Suspend(
  tag = Tag[Emit[String]], 
  input = "Hello",
  continue = Unit => Suspend(
    tag = Tag[Emit[String]],
    input = "World", 
    continue = Unit => Pure(())
  )
)
```

### Effect Types and Their Suspensions

| Effect | Input | Output to Resume |
|--------|-------|------------------|
| `Var[V]` | `op: Get \| Set[V] \| Update[V => V]` | `V` |
| `Emit[V]` | `v: V` | `Unit` |
| `Abort[E]` | `error: Result.Error[E]` | `Nothing` (never resumes) |
| `Env[R]` | `Unit` | `TypeMap[R]` |
| `Defer` | `Unit` | `Unit` |

**Suspensions are holes that need handlers to be resolved.** A `Suspend[Var[Int]]` is waiting for a handler to provide the element to continue between a `Op.Var[Int]` and `Int`. A `Suspend[Abort[E]]` is waiting for `Abort.run` to decide: "catch it or propagate it?"

## Rotation: Handling Multiple Effects

When you have multiple effects (`A < (Abort[E] & Emit[V] & Var[S])`), handlers must **rotate** through suspensions to find the ones they can handle.

```scala ignore
// Simplified handler logic (actual implementation in 01_miniKyo_core.scala)
def handle[I, O, E <: ArrowEffect[I, O], A, S](
  effect: Tag[E], 
  v: A < (S & E)
): Handle[I, O, E, A, S] = 
  def loop(curr: A < (S & E)): A < S =
    curr match
      case Pure(value) => Pure(value)
      
      // Found our effect - handle it!
      case suspend if suspend.tag <:< effect =>
        loop(handler(suspend.input, suspend.continue))
      
      // Not our effect - rotate: re-suspend and keep looking
      case suspend =>
        Suspend(suspend.tag, suspend.input, 
          output => loop(suspend.continue(output)))
  
  loop(v)
```

**Rotation** means: if this suspension isn't mine, wrap it back up and look deeper. This allows handlers to compose horizontally:

```scala ignore
// Conceptual example of horizontal composition
Env.run(config):           // handles Env[Config]
  Emit.runDiscard:          // handles Emit[Log]  
    Abort.run:              // handles Abort[String]
      program  // A < (Env[Config] & Emit[Log] & Abort[String])
```

Each handler peels off one effect, leaving the others untouched.

## Multiple Error Channels: A Real Example

Kyo's killer feature "à la carte effects", witch means we can have **distinct error types that compose**.

```scala
case class ValidationError(msg: String)
case class DbError(query: String, reason: String)
case class NetworkError(url: String, code: Int)

def validateUser(name: String): String < Abort[ValidationError] =
  if name.isEmpty then Abort.fail(ValidationError("Empty"))
  else name

def saveToDb(data: String): Unit < (Abort[DbError] & Sync) =
  if data.contains("invalid") 
  then Abort.fail(DbError("INSERT ...", "Invalid data"))
  else () 

def notify(user: String): Unit < (Abort[NetworkError] & Sync) =
  if user == "unreachable"
  then Abort.fail(NetworkError("https://api.notify", 503))
  else ()

// Compose all three error channels
def registerUser(name: String): Unit < (
  Abort[ValidationError] & 
  Abort[DbError] & 
  Abort[NetworkError] &
  Sync
) =
  for
    valid <- validateUser(name)
    _     <- saveToDb(valid)
    _     <- notify(valid)
  yield ()
```

### Why This Works: Type-Aware Tags

Without proper type information, `Abort[ValidationError]` and `Abort[DbError]` would be indistinguishable at runtime. Kyo (and this implementation via `izumi-reflect`) preserves full type parameters:

```scala ignore
assert(Tag[Abort[ValidationError]] != Tag[Abort[DbError]])
```

This means you can handle each error type independently:

```scala
// Handling each error type step by step (see 04_miniKyo_multiple_errors_example.scala)

def expr: Unit < (
  Abort[ValidationError] & 
  Abort[DbError] & 
  Abort[NetworkError] &
  Sync) = ???


// Result[ValidationError, Unit] < (...)
Abort.run[ValidationError](expr) 

val grouped: Result[DbError|NetworkError, Unit] < (Sync & Abort[ValidationError])= Abort.run[DbError|NetworkError](expr)
```

Each `Abort.run` peels off **one error type**, leaving the others in the effect stack. See `04_miniKyo_multiple_errors_example.scala` for a complete working example.

Note: It's better to factor error types into 2 - 3 types:
 
 - `Panic` (in Result.Panic(t: Throwable))
 - `Failure[ErrorType1]`
 - `Failure[ErrorType2]`

Kyo allows you to be more precise and explicit with errors.



## Custom Effects

Suspensions are just data. You can create custom effects for:

**Multishot**

```scala
sealed trait Choice[X] extends ArrowEffect[Seq[X], X]

object Choice:
  def eval[X: Tag](options: Seq[X]): X < Choice[X] =
    ArrowEffect.suspend(Tag[Choice[X]], options)

  def drop[X: Tag]: X < Choice[X] = eval(Nil)

  def run[A, X: Tag, S](v: A < (Choice[X] & S)): Seq[A] < S =
    ArrowEffect.handle(Tag[Choice[X]], v.map(x => Seq(x))): (input, cont) => 
        if(input.isEmpty) 
        then Nil
        else 
            input.map(cont).reduce: (x, y) => 
                x.map: seq1 => 
                    y.map: seq2 => 
                        seq1 ++ seq2
```

**Parsing with Backtracking**
```scala ignore
trait Parser[A] extends ArrowEffect[ParseError, A]

def parse[A](input: String): A < Parser[A] =
  ArrowEffect.suspend(Tag[Parser[A]], ParseError(input))
```

**Hypothesis Testing**
```scala ignore
trait Hypothesis extends ArrowEffect[Assumption, Validation]

def assume(condition: Boolean): Unit < Hypothesis =
  if !condition then 
    ArrowEffect.suspend(Tag[Hypothesis], Assumption(condition))
  else ()
```

Any computation that can be modeled as "pause here, let someone else decide" can be a suspension.


## Built-in Effects

```scala ignore
// These effects are defined in 01_miniKyo_core.scala
trait Abort[-E] extends ArrowEffect[Result.Error[E], Nothing]
trait Emit[-V]  extends ArrowEffect[V, Unit]
trait Env[+R]   extends ArrowEffect[Unit, R]
trait Var[V]    extends ArrowEffect[Var.Op[V], V]
trait Defer     extends ArrowEffect[Unit, Unit]

object Sync  // Abort[Nothing] & Defer
```

## Project Structure

- `01_miniKyo_core.scala` - Core types, suspension/rotation logic
- `02_miniKyo_kyoApp.scala` - Application runner
- `03_miniKyo_example.scala` - Basic example
- `04_miniKyo_multiple_errors_example.scala` - Multiple error channels
- `05_miniKyo_var.scala` - Stateful computations
- `99_miniKyo_result.scala` - Result[E, A] type
- `99_miniKyo_typeMap.scala` - Type-indexed map

## Running

```bash
# Compile project (including README code examples)
scala-cli compile . --power --enable-markdown

# Run tests
scala-cli test .

# Run examples
scala-cli run . --main-class miniKyo.Prg
scala-cli run . --main-class miniKyo.MultipleErrorsDemo
```

## Comparison to Real Kyo

This is an **educational implementation**. Real Kyo adds:
- Fiber-based concurrency
- Optimized runtime (no intermediate allocations), with a lot of inlining, when possible
- Many more effects (Async, Resource, Stream, Choice, etc.)
- Better error messages
- Custom tagging system (faster, less overhead, more use friendly (less tagging))
- ...

## References

- [Kyo](https://github.com/getkyo/kyo) - The real library
- [Kyo Laws](https://gist.github.com/fwbrasil/8c8b2b0236793391546c624cbbacd421) - Core principles
- [izumi-reflect](https://github.com/7mind/izumi-reflect) - Runtime type information
