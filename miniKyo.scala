//> using target.scope main

package miniKyo

// Gist from Flavio: Kyo Laws https://gist.github.com/fwbrasil/8c8b2b0236793391546c624cbbacd421

import scala.annotation.tailrec
import scala.reflect.ClassTag

// the signature in Kyo is a bit different
// ArrowEffect[-I[_], +O[_]], which allow for a lot more flexibility
// but it's not really necessary for this example
trait ArrowEffect[-I, +O]

trait Emit[-Value] extends ArrowEffect[Value, Unit]
trait Abort[-E]    extends ArrowEffect[E | Throwable, Nothing]
trait Env[+R]      extends ArrowEffect[Unit, TypeMap[R]] // It's a ContextEffect in Kyo, for more performance
trait Defer        extends ArrowEffect[Unit, Unit]

opaque type Sync <: (Abort[Nothing] & Defer) = (Abort[Nothing] & Defer)

object ArrowEffect:
  class Handle[I, O, E <: ArrowEffect[I, O], A, S](effect: Tag[E], v: A < (S & E)):
    def apply[B, S2](f: (input: I, cont: O => A < (E & S)) => A < (S & E & S2)): A < (S & S2) =
      v match
        case <.Pure(value)               => <.Pure(value)
        case s: <.Suspend[i, o, ?, a, ?] =>
          s.tag match
            case e if e == effect =>
              val res: A < (S & E & S2) = f(s.input.asInstanceOf[I], s.continue.asInstanceOf)
              handle(effect, res.asInstanceOf[A < (S & E)])(f)

            case _ =>
              <.Suspend[Any, Any, ArrowEffect[Any, Any], A, S](
                s.tag.asInstanceOf,
                s.input,
                s.continue.andThen(v => handle(effect, v)(f.asInstanceOf)).asInstanceOf
              ).asInstanceOf[A < (S & S2)]

  def handle[I, O, E <: ArrowEffect[I, O], A, S](effect: Tag[E], v: A < (S & E)): Handle[I, O, E, A, S] =
    Handle[I, O, E, A, S](effect, v)

  def suspend[I, O, E <: ArrowEffect[I, O]](effect: Tag[E], input: I): O < E =
    <.Suspend(effect, input, x => <.Pure(x))

  def suspendWith[I, O, E <: ArrowEffect[I, O]](effect: Tag[E], input: I)[A, S](f: O => A < S): A < (E & S) =
    <.Suspend(effect, input, f)

sealed trait <[+A, -S]:
  def map[B](f: A => B): B < S =
    this match
      case <.Pure(value)                   => <.Pure(f(value))
      case <.Suspend(tag, input, continue) =>
        <.Suspend(tag, input, continue andThen (_.map(f)))

  def flatMap[B, S2](f: A => B < S2): B < (S & S2) =
    this match
      case <.Pure(value)                   => f(value)
      case <.Suspend(tag, input, continue) =>
        <.Suspend(tag, input, continue andThen (_.flatMap(f)))

  infix def *>[B, S2](b: B < S2): B < (S & S2) = flatMap(_ => b)

object < {
  final case class Pure[A](value: A) extends <[A, Any]

  final case class Suspend[I, O, Effect <: ArrowEffect[I, O], A, S](
      tag: Tag[Effect],
      input: I,
      continue: O => A < S
  ) extends (A < (Effect & S))
}

object Kyo:

  def pure[A](value: A): A < Any = <.Pure(value)

  val unit: Unit < Any = pure(())

  def delay[A](thunk: => A): A < Sync =
    <.Suspend(
      Tag[Defer],
      (),
      Unit =>
        try pure(thunk)
        catch case e: Throwable => ArrowEffect.suspend(Tag[Abort[Nothing]], e)
    )

  def fail[E](e: E): Nothing < Abort[E]             = ArrowEffect.suspend(Tag[Abort[E]], e)
  def panic(e: Throwable): Nothing < Abort[Nothing] = ArrowEffect.suspend(Tag[Abort[Nothing]], e)

  def printline(line: String): Unit < Sync = delay(System.out.println(line))

  def emit[Value](value: Value): Unit < Emit[Value] = ArrowEffect.suspend(Tag[Emit[Value]], value)

  def log(msg: String): Unit < Emit[String] = emit(msg)

  def get[R: Tag as tag]: R < Env[R] =
    ArrowEffect.suspendWith(Tag[Env[R]], ())(typeMap => pure(typeMap.get[R]))

  class Use[R: Tag]:
    def apply[A, S](f: R => A < S): A < (S & Env[R]) =
      ArrowEffect.suspendWith(Tag[Env[R]], ())(typeMap => f(typeMap.get[R]))

  def use[R: Tag]: Use[R] = Use[R]

  def readLine: String < (Sync & Abort[Throwable]) = delay(scala.io.StdIn.readLine())

object Emit:
  def runDiscard[A, S, V](v: A < (S & Emit[V])): A < S =
    runForeach(v)(_ => Kyo.unit)

  def runForeach[A, S, V](v: A < (S & Emit[V]))[S2](f: V => Any < S2): A < (S & S2) =
    ArrowEffect.handle(effect = Tag[Emit[V]], v)((input, cont) => f(input) *> cont(()))

object Env:
  def run[A, S, R: Tag](r: R)(v: A < (S & Env[R])): A < S =
    ArrowEffect.handle(Tag[Env[R]], v)((input, cont) => cont(TypeMap(r)))

// --- --- --- --- ---

val body =
  val hello: Unit < Sync                                 = Kyo.printline("Hello world!")
  val start: Unit < Emit[String]                         = Kyo.log("Starting...") *> Kyo.log("... for real")
  val whatIsYourName: String < (Sync & Abort[Throwable]) = Kyo.printline("what is you name ?") *> Kyo.readLine

  start *> hello *> whatIsYourName.flatMap(name => Kyo.printline(s"Hello $name!"))

case class Config(logToConsole: Boolean)

def logStrat[A, S](v: A < (S & Emit[String])): A < (S & Env[Config] & Sync) =
  Kyo.use[Config]: config =>
    if config.logToConsole
    then Emit.runForeach(v)(s => Kyo.printline("log: " + s))
    else Emit.runDiscard(v)

object Prg extends KyoApp:
  run:
    Env.run(Config(logToConsole = true)):
      logStrat:
        body

trait Var[Value] extends ArrowEffect[Var.internal.Op[Value], Value]

object Var:
  object internal:
    enum Op[V]:
      case Get()
      case Set(value: V)
      case Update(f: V => V)

  def get[V]: V < Var[V]               = ArrowEffect.suspend(Tag[Var[V]], Var.internal.Op.Get())
  def set[V](value: V): V < Var[V]     = ArrowEffect.suspend(Tag[Var[V]], Var.internal.Op.Set(value))
  def update[V](f: V => V): V < Var[V] = ArrowEffect.suspend(Tag[Var[V]], Var.internal.Op.Update(f))

  class Run[V](value: V):
    def apply[A, S](v: A < (S & Var[V])): A < S =
      ArrowEffect.handle(Tag[Var[V]], v)((input, cont) =>

        val newVal: V = input match
          case Var.internal.Op.Get()     => value
          case Var.internal.Op.Set(v)    => v
          case Var.internal.Op.Update(f) => f(value)

        // Not optimal, need a better way to do this, with "ArrowEffect.handleLoop"
        Run(newVal)(cont(newVal))
      )

  def run[V](value: V): Run[V] = Run[V](value)

trait KyoApp:
  private var runs: List[Any < (Abort[Throwable] & Sync)] = Nil

  @tailrec
  private def unsafeRun[A](v: A < (Abort[Throwable] & Sync)): A =
    v match
      case <.Pure(value)               => value
      case s: <.Suspend[?, ?, ?, ?, ?] =>
        s.tag match
          case t if t == Tag[Abort[Throwable]] => throw s.input.asInstanceOf[Throwable]
          case t if t == Tag[Defer]            => unsafeRun(s.continue(().asInstanceOf))
          case t                               => throw new Exception(s"unexpected effect: $t")

  def run[A](v: A < (Abort[Throwable] & Sync)): Unit =
    runs = v :: runs

  def main(args: Array[String]): Unit =
    runs.reverse.foreach(unsafeRun)
