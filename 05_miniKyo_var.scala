package miniKyo

// ---  creating a new effect [Var] (for monadic state operations)

trait Var[Value] extends ArrowEffect[Var.Op[Value], Value]

object Var:
  enum Op[V]:
    case Get()
    case Set(value: V)
    case Update(f: V => V)

  def get[V: Tag]: V < Var[V]               = ArrowEffect.suspend(Tag[Var[V]], Var.Op.Get())
  def set[V: Tag](value: V): V < Var[V]     = ArrowEffect.suspend(Tag[Var[V]], Var.Op.Set(value))
  def update[V: Tag](f: V => V): V < Var[V] = ArrowEffect.suspend(Tag[Var[V]], Var.Op.Update(f))

  class Run[V: Tag](value: V):
    def apply[A, S](v: A < (S & Var[V])): A < S =
      ArrowEffect.handle(Tag[Var[V]], v)((input, cont) =>

        val newVal: V = input match
          case Var.Op.Get()     => value
          case Var.Op.Set(v)    => v
          case Var.Op.Update(f) => f(value)

        // Not optimal, need a better way to do this, with "ArrowEffect.handleLoop"
        Run(newVal)(cont(newVal))
      )

  def run[V: Tag](value: V): Run[V] = Run[V](value)
