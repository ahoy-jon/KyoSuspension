package miniKyo

import java.io.IOException

val body =
  val hello: Unit < Sync                                   = Console.printline("Hello world!")
  val start: Unit < Emit[Log]                              = Log.log("Starting...") *> Log.log("... for real")
  val whatIsYourName: String < (Sync & Abort[IOException]) = Console.printline("what is you name ?") *> Console.readLine

  start *> hello *> whatIsYourName.map(name => Console.printline(s"Hello $name!"))

case class Config(logToConsole: Boolean)

def logStrat[A, S](v: A < (S & Emit[Log])): A < (S & Env[Config] & Sync) =
  Env.use[Config]: config =>
    if config.logToConsole
    then Emit.runForeach(v)(log => Console.printline(s"[${log.level}] ${log.msg}"))
    else Emit.runDiscard(v)

object Prg extends KyoApp:
  run:
    Env.run(Config(logToConsole = true)):
      logStrat:
        body
