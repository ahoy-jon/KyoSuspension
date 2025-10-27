package miniKyo

import scala.annotation.tailrec
import scala.language.implicitConversions
import java.io.IOException
import izumi.reflect.Tag

// Export Tag so it's available throughout the miniKyo package
export izumi.reflect.Tag

// the signature in Kyo is a bit different
// ArrowEffect[-I[_], +O[_]], which allow for a lot more flexibility
// but it's not really necessary for this example
trait ArrowEffect[-I, +O]

// Note: they are never instantiated, they are markers
trait Emit[-Value] extends ArrowEffect[Value, Unit]
trait Abort[-E]    extends ArrowEffect[Result.Error[E], Nothing]
trait Env[+R]      extends ArrowEffect[Unit, TypeMap[R]] // It's a ContextEffect in Kyo, for more performance
trait Defer        extends ArrowEffect[Unit, Unit]

opaque type Sync <: (Abort[Nothing] & Defer) = (Abort[Nothing] & Defer)

sealed trait <[+A, -S]:
  def map[B, S2](f: A => B < S2): B < (S & S2)

  // useful for chaining using for-comprehensions
  final def flatMap[B, S2](f: A => B < S2): B < (S & S2) = map(f)

  final infix def *>[B, S2](b: B < S2): B < (S & S2) = flatMap(_ => b)

object < :

  // needed to merge map and flatMap definitions
  implicit def autoLift[A](a: A): A < Any = Pure(a)

  extension [A](v: A < Any)
    def eval: A = v match
      case <.Pure(value) => value

  final case class Pure[A](value: A) extends <[A, Any]:
    def map[B, S2](f: A => B < S2): B < S2 = f(value)

  final case class Suspend[I, O, Effect <: ArrowEffect[I, O], A, S](
      tag: Tag[Effect],
      input: I,
      continue: O => A < S
  ) extends (A < (Effect & S)):
    def map[B, S2](f: A => B < S2): B < (Effect & S & S2) =
      Suspend(tag, input, continue andThen (_.map(f)))

object Sync:
  def delay[A](thunk: => A): A < Sync =
    ArrowEffect.suspendWith(Tag[Defer], ()): Unit =>
      try thunk
      catch
        case e: Throwable =>
          ArrowEffect.suspend(Tag[Abort[Nothing]], Result.Panic(e))

object Abort:
  def fail[E: Tag](e: E): Nothing < Abort[E]        = ArrowEffect.suspend(Tag[Abort[E]], Result.Failure(e))
  def panic(e: Throwable): Nothing < Abort[Nothing] = ArrowEffect.suspend(Tag[Abort[Nothing]], Result.Panic(e))

  class Run[E: Tag]:
    def apply[A, S](v: A < (S & Abort[E])): Result[E, A] < S =
      val lifted: Result[E, A] < (S & Abort[E]) = v.map(Result.Success.apply)
      ArrowEffect.handle(Tag[Abort[E]], lifted)((input, cont) => input) // we don't continue if there's an error

  def run[E: Tag]: Run[E] = Run[E]

object Emit:
  def value[Value: Tag](value: Value): Unit < Emit[Value] =
    ArrowEffect.suspend(Tag[Emit[Value]], value)

  def runDiscard[A, S, V: Tag](v: A < (S & Emit[V])): A < S =
    runForeach(v)(_ => Kyo.unit)

  class RunForeach[A, S, V: Tag](v: A < (S & Emit[V])):
    def apply[S2](f: V => Unit < S2): A < (S & S2) =
      ArrowEffect.handle(effect = Tag[Emit[V]], v)((input, cont) => f(input) *> cont(()))

  def runForeach[A, S, V: Tag](v: A < (S & Emit[V])): RunForeach[A, S, V] = RunForeach[A, S, V](v)

object Env:
  def get[R: Tag as tag]: R < Env[R] =
    ArrowEffect.suspendWith(Tag[Env[R]], ())(typeMap => typeMap.get[R])

  class Use[R: Tag]:
    def apply[A, S](f: R => A < S): A < (S & Env[R]) =
      ArrowEffect.suspendWith(Tag[Env[R]], ())(typeMap => f(typeMap.get[R]))

  def use[R: Tag]: Use[R] = Use[R]

  def run[A, S, R: Tag](r: R)(v: A < (S & Env[R])): A < S =
    ArrowEffect.handle(Tag[Env[R]], v)((input, cont) => cont(TypeMap(r)))

object Kyo:
  def pure[A](value: A): A < Any = <.Pure(value)

  val unit: Unit < Any = pure(())

object Console:
  def printline(line: String): Unit < Sync =
    Sync.delay(System.out.println(line))

  def readLine: String < (Sync & Abort[IOException]) =
    Sync.delay(scala.io.StdIn.readLine())

case class Log(level: Log.Level, msg: String)

object Log:
  enum Level:
    case DEBUG
    case INFO
    case WARN
    case ERROR

  def log(msg: String, level: Log.Level = Log.Level.INFO): Unit < Emit[Log] =
    Emit.value(Log(level = level, msg = msg))

object ArrowEffect:
  class Handle[I, O, E <: ArrowEffect[I, O], A, S](effect: Tag[E], v: A < (S & E)):
    def apply[B, S2](f: (input: I, cont: O => A < (E & S)) => A < (S & E & S2)): A < (S & S2) =
      type IX
      type OX
      type EX <: ArrowEffect[IX, OX]

      extension [SX](v: A < (SX & EX)) def cleanEx: A < SX = v.asInstanceOf[A < SX]

      def loop(curr: A < (S & S2 & E)): A < (S & S2 & EX) =
        curr match
          case <.Pure(value) => <.Pure(value)

          case suspend: <.Suspend[I, O, E, A, S & E] @unchecked if effect <:< suspend.tag =>
            loop(f(suspend.input, suspend.continue))

          // rotation, the effect E will be handled later in the computation
          case suspend: <.Suspend[IX, OX, EX, A, ?] @unchecked =>
            <.Suspend(
              suspend.tag,
              suspend.input,
              output => loop(suspend.continue(output))
            )

      loop(v).cleanEx

  def handle[I, O, E <: ArrowEffect[I, O], A, S](effect: Tag[E], v: A < (S & E)): Handle[I, O, E, A, S] =
    Handle[I, O, E, A, S](effect, v)

  def suspend[I, O, E <: ArrowEffect[I, O]](effect: Tag[E], input: I): O < E =
    <.Suspend(effect, input, x => <.Pure(x))

  class SuspendWith[I, O, E <: ArrowEffect[I, O]](effect: Tag[E], input: I):
    def apply[A, S](f: O => A < S): A < (E & S) =
      <.Suspend(effect, input, f)

  def suspendWith[I, O, E <: ArrowEffect[I, O]](effect: Tag[E], input: I): SuspendWith[I, O, E] =
    SuspendWith[I, O, E](effect, input)
