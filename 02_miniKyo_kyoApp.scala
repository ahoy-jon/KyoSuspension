// File: 02_miniKyo_kyoApp.scala (note: moved to KyoApp.scala for clarity)

package miniKyo

trait KyoApp:
  private var runs: List[Any < (Abort[Throwable] & Sync)] = Nil

  private def unsafeRun[A](v: A < (Abort[Throwable] & Sync)): A = {
    val throwing = ArrowEffect.handle(Tag[Abort[Throwable]], v): (error, cont) =>
      throw error.toThrowable

    ArrowEffect.handle(Tag[Defer], throwing.asInstanceOf[A < Defer])((input, cont) => cont(())).eval
  }

  def run[A](v: A < (Abort[Throwable] & Sync)): Unit =
    runs = v :: runs

  def main(args: Array[String]): Unit =
    runs.reverse.foreach(unsafeRun)
