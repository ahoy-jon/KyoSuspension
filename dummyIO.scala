//> using target.scope main

package likeKyo

import java.io.IOException
import scala.annotation.tailrec
import scala.util.Failure
import scala.util.Try

enum IO[-R, +Error, +A, +Emit]:
  case Pure(value: A)
  case Delay(continue: () => IO[R, Error, A, Emit])
  case Abort(e: Error | Throwable)
  case Env(r: R => IO[R, Error, A, Emit])
  case Emit(value: Emit, continue: () => IO[R, Error, A, Emit])

  def map[B](f: A => B): IO[R, Error, B, Emit] = flatMap(a => Pure(f(a)))

  def flatMap[B, E2 >: Error, R2 <: R, Emit2 >: Emit](f: A => IO[R2, E2, B, Emit2]): IO[R2, E2, B, Emit2] = {
    this match
      case IO.Pure(value)           => f(value)
      case IO.Delay(continue)       => IO.Delay(() => continue().flatMap(f))
      case IO.Abort(e)              => IO.Abort(e)
      case IO.Env(r)                => IO.Env(r.andThen(_.flatMap(f)))
      case IO.Emit(value, continue) =>
        IO.Emit(value, () => continue().flatMap(f))
  }

  def handleEmit[E2 >: Error, R2 <: R, Emit2](
      f: Emit => IO[R2, E2, Unit, Emit2]
  ): IO[R2, E2, A, Emit2] =
    this match
      case IO.Pure(value)           => IO.Pure(value)
      case IO.Delay(continue)       => IO.Delay(() => continue().handleEmit(f))
      case IO.Abort(e)              => IO.Abort(e)
      case IO.Env(r)                => IO.Env(r.andThen(_.handleEmit(f)))
      case IO.Emit(value, continue) =>
        f(value).flatMap(_ => continue().handleEmit(f))

  def discardEmit: IO[R, Error, A, Nothing] =
    this match
      case IO.Pure(value)           => IO.Pure(value)
      case IO.Delay(continue)       => IO.Delay(() => continue().discardEmit)
      case IO.Abort(e)              => IO.Abort(e)
      case IO.Env(r)                => IO.Env(r.andThen(_.discardEmit))
      case IO.Emit(value, continue) => continue().discardEmit

  def either: IO[R, Nothing, Either[Error, A], Emit] =
    this match
      case IO.Pure(value)           => Pure(Right(value))
      case IO.Delay(continue)       => Delay(() => continue().either)
      case IO.Abort(e:Error)        => Pure(Left(e))
      case IO.Abort(e:Throwable)    => IO.Abort(e)
      case IO.Env(r)                => Env(r.andThen(_.either))
      case IO.Emit(value, continue) => Emit(value, () => continue().either)

  def resurrect(using Error <:< Throwable): IO[R, Nothing, Try[A], Emit] =
    this match
      case IO.Pure(value)           => Pure(Try(value))
      case IO.Delay(continue)       => Delay(() => continue().resurrect)
      case IO.Abort(e: Error)       => Pure(Failure(e))
      case IO.Abort(e: Throwable)   => Pure(Failure(e))
      case IO.Env(r)                => Env(r.andThen(_.resurrect))
      case IO.Emit(value, continue) => Emit(value, () => continue().resurrect)

  def provide(r: R): IO[Any, Error, A, Emit] =
    this match
      case IO.Pure(value)           => Pure(value)
      case IO.Delay(continue)       => Delay(() => continue().provide(r))
      case IO.Abort(e)              => Abort(e)
      case IO.Env(read)             => read(r).provide(r)
      case IO.Emit(value, continue) => Emit(value, () => continue().provide(r))

  infix def *>[R2 <: R, E2 >: Error, A2, Emit2 >: Emit](
      that: IO[R2, E2, A2, Emit2]
  ): IO[R2, E2, A2, Emit2] =
    flatMap(_ => that)

object IO:
  def unit: IO[Any, Nothing, Unit, Nothing] = Pure(())

  def delay[A](thunk: => A): IO[Any, Nothing, A, Nothing] =
    IO.Delay(() => IO.Pure(thunk))

  def println(line: String): IO[Any, Nothing, Unit, Nothing] =
    delay(System.out.println(line))

  def log(msg: String): IO[Any, Nothing, Unit, String] =
    IO.Emit(msg, () => unit)

  def readLine: IO[Any, IOException, String, Nothing] =
    delay(scala.io.StdIn.readLine())

  trait Access[R]:
    def apply[A, E, Emit](f: R => IO[R, E, A, Emit]): IO[R, E, A, Emit] = IO.Env(f)

  // old way to avoid a problem with intelliJ
  def access[R]: Access[R] = new Access[R] {}

  @tailrec
  def unsafeRun[A](io: IO[Any, Throwable, A, Nothing]): A =
    io match
      case IO.Pure(value)           => value
      case IO.Delay(continue)       => unsafeRun(continue())
      case IO.Abort(e)              => throw e
      case IO.Env(r)                => unsafeRun(r(()))
      case IO.Emit(value, continue) => unsafeRun(continue())

case class Config(logToConsole: Boolean)

val expr =
  val hello          = IO.println("Hello world!")
  val start          = IO.log("Starting...")
  val whatIsYourName = IO.println("what is you name ?") *> IO.readLine

  start *> hello *> whatIsYourName.flatMap(name => IO.println(s"Hello $name!"))

def logStrat[A, E](io: IO[Any, E, A, String]): IO[Config, E, A, Nothing] =
  IO.access[Config]: config =>
    if config.logToConsole
    then io.handleEmit(str => IO.println("log: " + str))
    else io.discardEmit

val prg = logStrat(expr)

@main
def main(): Unit =
  IO.unsafeRun(prg.provide(Config(logToConsole = true)))
  println("-" * 10)
  IO.unsafeRun(prg.provide(Config(logToConsole = false)))
