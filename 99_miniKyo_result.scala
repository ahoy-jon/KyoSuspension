// File: 99_miniKyo_result.scala

package miniKyo

import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}

/** Result type that can represent either a success or an error.
  *
  * @tparam E
  *   The error type
  * @tparam A
  *   The success type
  */
sealed trait Result[+E, +A]

object Result:
  final case class Success[+A](value: A) extends Result[Nothing, A]

  sealed trait Error[+E] extends Result[E, Nothing]:
    final def toThrowable(using ev: E <:< Throwable): Throwable = this match
      case Failure(e) => ev(e)
      case Panic(t)   => t

    final def fold[B](onFailure: E => B, onPanic: Throwable => B): B = this match
      case Failure(e) => onFailure(e)
      case Panic(t)   => onPanic(t)

  final case class Failure[+E](error: E) extends Error[E]

  final case class Panic(throwable: Throwable) extends Error[Nothing]

  extension [E, A](result: Result[E, A])
    def isSuccess: Boolean = result match
      case Success(_)  => true
      case _: Error[?] => false

    def isError: Boolean = !result.isSuccess

    def toEither: Either[Error[E], A] = result match
      case Success(value)             => Right(value)
      case error: Error[E] @unchecked => Left(error)

    def getOrElse[B >: A](default: => B): B = result match
      case Success(value) => value
      case _: Error[?]    => default

    def map[B](f: A => B): Result[E, B] = result match
      case Success(value)             => Success(f(value))
      case error: Error[E] @unchecked => error

    def flatMap[E2 >: E, B](f: A => Result[E2, B]): Result[E2, B] = result match
      case Success(value)             => f(value)
      case error: Error[E] @unchecked => error

  def success[A](value: A): Result[Nothing, A] = Success(value)

  def failure[E](error: E): Result[E, Nothing] = Failure(error)

  def panic(throwable: Throwable): Result[Nothing, Nothing] = Panic(throwable)

  def fromEither[E, A](either: Either[E, A]): Result[E, A] = either match
    case Right(value) => Success(value)
    case Left(error)  => failure(error)

  def fromTry[A](t: Try[A]): Result[Throwable, A] = t match
    case TrySuccess(value) => Success(value)
    case TryFailure(error) => failure(error)

end Result
