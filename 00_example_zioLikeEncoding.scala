package likeZIO

import java.io.IOException
import scala.annotation.tailrec
import scala.util.Failure
import scala.util.Try

enum LikeZIO[-R, +Error, +A, +Emit]:
  case Pure(value: A)
  case Defer(continue: () => LikeZIO[R, Error, A, Emit])
  case Abort(e: Error | Throwable)
  case Env(r: R => LikeZIO[R, Error, A, Emit])
  case Emit(value: Emit, continue: () => LikeZIO[R, Error, A, Emit])

  def map[B](f: A => B): LikeZIO[R, Error, B, Emit] = flatMap(a => Pure(f(a)))

  final def flatMap[B, E2 >: Error, R2 <: R, Emit2 >: Emit](f: A => LikeZIO[R2, E2, B, Emit2]): LikeZIO[R2, E2, B, Emit2] = {
    this match
      case LikeZIO.Pure(value)           => f(value)
      case LikeZIO.Defer(continue)       => LikeZIO.Defer(() => continue().flatMap(f))
      case LikeZIO.Abort(e)              => LikeZIO.Abort(e)
      case LikeZIO.Env(r)                => LikeZIO.Env(r.andThen(_.flatMap(f)))
      case LikeZIO.Emit(value, continue) =>
        LikeZIO.Emit(value, () => continue().flatMap(f))
  }

  final def handleEmit[E2 >: Error, R2 <: R, Emit2](
      f: Emit => LikeZIO[R2, E2, Unit, Emit2]
  ): LikeZIO[R2, E2, A, Emit2] =
    this match
      case LikeZIO.Pure(value)           => LikeZIO.Pure(value)
      case LikeZIO.Defer(continue)       => LikeZIO.Defer(() => continue().handleEmit(f))
      case LikeZIO.Abort(e)              => LikeZIO.Abort(e)
      case LikeZIO.Env(r)                => LikeZIO.Env(r.andThen(_.handleEmit(f)))
      case LikeZIO.Emit(value, continue) =>
        f(value).flatMap(_ => continue().handleEmit(f))

  final def discardEmit: LikeZIO[R, Error, A, Nothing] =
    this match
      case LikeZIO.Pure(value)           => LikeZIO.Pure(value)
      case LikeZIO.Defer(continue)       => LikeZIO.Defer(() => continue().discardEmit)
      case LikeZIO.Abort(e)              => LikeZIO.Abort(e)
      case LikeZIO.Env(r)                => LikeZIO.Env(r.andThen(_.discardEmit))
      case LikeZIO.Emit(value, continue) => continue().discardEmit

  final def either: LikeZIO[R, Nothing, Either[Error, A], Emit] =
    this match
      case LikeZIO.Pure(value)                => Pure(Right(value))
      case LikeZIO.Defer(continue)            => Defer(() => continue().either)
      case LikeZIO.Abort(e: Throwable)        => LikeZIO.Abort(e)
      case LikeZIO.Abort(e: Error @unchecked) => Pure(Left(e))
      case LikeZIO.Env(r)                     => Env(r.andThen(_.either))
      case LikeZIO.Emit(value, continue)      => Emit(value, () => continue().either)

  final def resurrect(using Error <:< Throwable): LikeZIO[R, Nothing, Try[A], Emit] =
    this match
      case LikeZIO.Pure(value)                => Pure(Try(value))
      case LikeZIO.Defer(continue)            => Defer(() => continue().resurrect)
      case LikeZIO.Abort(e: Throwable)        => Pure(Failure(e))
      case LikeZIO.Abort(e: Error @unchecked) => Pure(Failure(e))
      case LikeZIO.Env(r)                     => Env(r.andThen(_.resurrect))
      case LikeZIO.Emit(value, continue)      => Emit(value, () => continue().resurrect)

  def provide(r: R): LikeZIO[Any, Error, A, Emit] =
    this match
      case LikeZIO.Pure(value)           => Pure(value)
      case LikeZIO.Defer(continue)       => Defer(() => continue().provide(r))
      case LikeZIO.Abort(e)              => Abort(e)
      case LikeZIO.Env(read)             => read(r).provide(r)
      case LikeZIO.Emit(value, continue) => Emit(value, () => continue().provide(r))

  infix def *>[R2 <: R, E2 >: Error, A2, Emit2 >: Emit](
      that: LikeZIO[R2, E2, A2, Emit2]
  ): LikeZIO[R2, E2, A2, Emit2] =
    flatMap(_ => that)

object LikeZIO:
  def unit: LikeZIO[Any, Nothing, Unit, Nothing] = Pure(())

  def delay[A](thunk: => A): LikeZIO[Any, Nothing, A, Nothing] =
    LikeZIO.Defer(() => LikeZIO.Pure(thunk))

  def println(line: String): LikeZIO[Any, Nothing, Unit, Nothing] =
    delay(System.out.println(line))

  def log(msg: String): LikeZIO[Any, Nothing, Unit, String] =
    LikeZIO.Emit(msg, () => unit)

  def readLine: LikeZIO[Any, IOException, String, Nothing] =
    delay(scala.io.StdIn.readLine())

  class Use[R]:
    def apply[A, E, Emit](f: R => LikeZIO[R, E, A, Emit]): LikeZIO[R, E, A, Emit] = LikeZIO.Env(f)

  def use[R]: Use[R] = Use[R]

  @tailrec
  def unsafeRun[A](io: LikeZIO[Any, Throwable, A, Nothing]): A =
    io match
      case LikeZIO.Pure(value)           => value
      case LikeZIO.Defer(continue)       => unsafeRun(continue())
      case LikeZIO.Abort(e)              => throw e
      case LikeZIO.Env(r)                => unsafeRun(r(()))
      case LikeZIO.Emit(value, continue) => unsafeRun(continue())

// --- --- --- --- ---

object Example {

  case class Config(logToConsole: Boolean)

  val expr: LikeZIO[Any, IOException, Unit, String] =
    val hello          = LikeZIO.println("Hello world!")
    val start          = LikeZIO.log("Starting...")
    val whatIsYourName = LikeZIO.println("what is you name ?") *> LikeZIO.readLine

    start *> hello *> whatIsYourName.flatMap(name => LikeZIO.println(s"Hello $name!"))

  def logStrat[A, E](io: LikeZIO[Any, E, A, String]): LikeZIO[Config, E, A, Nothing] =
    LikeZIO.use[Config]: config =>
      if config.logToConsole
      then io.handleEmit(str => LikeZIO.println("log: " + str))
      else io.discardEmit

  val prg: LikeZIO[Config, IOException, Unit, Nothing] = logStrat(expr)

  def main(args: Array[String]): Unit =
    LikeZIO.unsafeRun(prg.provide(Config(logToConsole = true)))
    println("-" * 10)
    LikeZIO.unsafeRun(prg.provide(Config(logToConsole = false)))

}
