package miniKyo

// FROM https://raw.githubusercontent.com/getkyo/kyo/refs/heads/main/kyo-kernel/shared/src/main/scala/kyo/kernel/Isolate.scala

/** Mechanisms for handling pending effects when forking computations.
  *
  * Isolate enables proper state management across execution boundaries. When forking execution, effects require special handling to prevent state
  * leakage and ensure consistency.
  *
  * The abstraction employs three type parameters to control effect flow:
  *   - `Remove`: Effects that will be satisfied (handled) by the isolation
  *   - `Keep`: Effects that remain available during isolated execution
  *   - `Restore`: Effects that become available after isolation completes
  *
  * @tparam Remove
  *   Effects that will be satisfied (handled) by the isolation
  * @tparam Keep
  *   Effects that remain available during isolated execution
  * @tparam Restore
  *   Effects that become available after isolation completes
  */
abstract class Isolate[Remove, -Keep, -Restore]:
  self =>

  /** The type of state being managed */
  type State

  /** How state is transformed during isolated execution */
  type Transform[_]

  /** Captures the current state for isolation.
    *
    * This is the first phase of isolation, obtaining the state that will be managed during the isolated execution.
    *
    * @param f
    *   Function that receives the captured state
    * @return
    *   Computation with Remove, Keep, and additional effects
    */
  def capture[A, S](f: State => A < S): A < (Remove & Keep & S)

  /** Executes a computation with isolated state.
    *
    * This is the second phase where the computation runs in an isolated context. Only Keep effects and additional effects S are available - Remove
    * effects have been captured and isolated.
    *
    * @param state
    *   The captured state from phase 1
    * @param v
    *   The computation to run in isolation
    * @return
    *   Transformed result with only Keep and additional effects
    */
  def isolate[A, S](state: State, v: A < (S & Remove)): Transform[A] < (Keep & S)

  /** Restores state after isolated execution.
    *
    * This is the final phase that determines how the transformed state is propagated back.
    *
    * @param v
    *   The transformed computation from phase 2
    * @return
    *   Final result with Restore and additional effects
    */
  def restore[A, S](v: Transform[A] < S): A < (Restore & S)

  /** Runs a computation with full state lifecycle management.
    *
    * Convenience method that composes all three phases: capture, isolate, and restore.
    *
    * @param v
    *   The computation to run with isolation
    * @return
    *   Result with original Remove effects handled and Restore effects available
    */
  final def run[A, S](v: A < (S & Remove)): A < (S & Remove & Keep & Restore) =
    capture(state => restore(isolate(state, v)))

  /** Applies this isolate to a computation that requires it. */
  final def use[A](f: this.type ?=> A): A = f(using this)

  /** Composes this isolate with another, managing both states.
    *
    * Creates a new isolate that handles the state lifecycles of both isolates, maintaining proper ordering and effect tracking.
    */
  final def andThen[RemoveSecond, KeepSecond, RestoreSecond](
      next: Isolate[RemoveSecond, KeepSecond, RestoreSecond]
  ): Isolate[Remove & RemoveSecond, Keep & KeepSecond, Restore & RestoreSecond] =
    Isolate.andThen(self, next)

end Isolate

object Isolate:

  def andThen[RemoveFirst, KeepFirst, RestoreFirst, RemoveSecond, KeepSecond, RestoreSecond](
      first: Isolate[RemoveFirst, KeepFirst, RestoreFirst],
      second: Isolate[RemoveSecond, KeepSecond, RestoreSecond]
  ): Isolate[RemoveFirst & RemoveSecond, KeepFirst & KeepSecond, RestoreFirst & RestoreSecond] =
    first match
      case Identity => second
      case _        =>
        second match
          case Identity => first
          case _        => AndThen(first, second)

  /** Gets the Isolate instance for given effect types. */
  def apply[Remove, Keep, Restore](using isolate: Isolate[Remove, Keep, Restore]): Isolate[Remove, Keep, Restore] = isolate

  /** No-op isolate that performs no state management.
    *
    * Used as a base case for isolate composition and when no isolation is needed.
    */
  object Identity extends Isolate[Any, Any, Any]:
    type State        = Unit
    type Transform[A] = A

    def capture[A, S](f: State => A < S)              = f(())
    def isolate[A, S](state: State, v: A < (S & Any)) = v
    def restore[A, S](v: A < S)                       = v
  end Identity

  /** Composition of two isolates, managing both state lifecycles.
    *
    * @param left
    *   The first isolate to apply
    * @param right
    *   The second isolate to apply
    */
  final class AndThen[RemoveFirst, KeepFirst, RestoreFirst, RemoveSecond, KeepSecond, RestoreSecond](
      val first: Isolate[RemoveFirst, KeepFirst, RestoreFirst],
      val second: Isolate[RemoveSecond, KeepSecond, RestoreSecond]
  ) extends Isolate[RemoveFirst & RemoveSecond, KeepFirst & KeepSecond, RestoreFirst & RestoreSecond]:

    type Remove  = RemoveFirst & RemoveSecond
    type Keep    = KeepFirst & KeepSecond
    type Restore = RestoreFirst & RestoreSecond

    case class State(_first: first.State, _second: second.State)

    type Transform[A] = first.Transform[second.Transform[A]]

    def capture[A, S](f: State => A < S): A < (Remove & Keep & S) =
      first.capture(s1 => second.capture(s2 => f(State(s1, s2))))

    def isolate[A, S](state: State, v: A < (S & Remove)): Transform[A] < (Keep & S) =
      first.isolate(state._first, second.isolate(state._second, v))

    def restore[A, S](v: Transform[A] < S): A < (Restore & S) =
      second.restore(first.restore(v))

  end AndThen

end Isolate

extension (varType: Var.type)
  // usualy isolate is defined directy on the Effect companion object, but we want to add it to the Var companion object
  def isolate: VarIsolate.type = VarIsolate

object VarIsolate:

  def lastUpdate[V: Tag]: Isolate[Var[V], Any, Var[V]] = LastUpdate[V]()

  def discard[V: Tag]: Isolate[Var[V], Any, Any] = Discard[V]()

  def conditionalUpdate[E: Tag, V: Tag](discardVarUpdatesOnFailure: E => Boolean): Isolate[Var[V] & Abort[E], Any, Var[V] & Abort[E]] =
    ConditionalUpdate[V, E](discardVarUpdatesOnFailure)

  /** Isolate strategy that updates the final value after isolated execution.
    *
    * This captures the initial state, runs the isolated computation, and restores the final state.
    */
  final class LastUpdate[V: Tag] extends Isolate[Var[V], Any, Var[V]]:
    type State        = V
    type Transform[A] = (V, A)

    def capture[A, S](f: State => A < S): A < (Var[V] & S) =
      Var.get[V].map(f)

    def isolate[A, S](state: State, v: A < (S & Var[V])): Transform[A] < S =
      Var.run(state)(v.map(a => Var.get[V].map(finalState => (finalState, a))))

    def restore[A, S](v: Transform[A] < S): A < (Var[V] & S) =
      v.map: (finalState, a) =>
        Var.set(finalState) *> a

  end LastUpdate

  /** Isolate strategy that discards changes made during isolated execution.
    *
    * This captures the initial state, runs the isolated computation, and restores the initial state (ignoring any changes).
    */
  final class Discard[V: Tag] extends Isolate[Var[V], Any, Any]:
    type State        = V
    type Transform[A] = A

    def capture[A, S](f: State => A < S): A < (Var[V] & S) = Var.get[V].map(f)

    def isolate[A, S](state: State, v: A < (S & Var[V])): Transform[A] < S = Var.run(state)(v)

    def restore[A, S](v: Transform[A] < S): A < S = v
  end Discard

  /** Isolate strategy for Var[V] & Abort[E] that conditionally updates based on error predicate.
    *
    * If the computation succeeds, the Var is updated. If it fails with an error matching the predicate, the Var update is discarded. Otherwise, the
    * Var is updated even on failure.
    *
    * @param discardVarUpdatesOnFailure
    *   Predicate determining whether to discard updates to Var
    */
  final class ConditionalUpdate[V, E](discardVarUpdatesOnFailure: E => Boolean)(using tagV: Tag[V], tagE: Tag[E])
      extends Isolate[Var[V] & Abort[E], Any, Var[V] & Abort[E]]:
    type State        = V
    type Transform[A] = (V, Result[E, A])

    def capture[A, S](f: State => A < S): A < (Var[V] & S) =
      Var.get[V](using tagV).map(f)

    def isolate[A, S](state: State, v: A < (S & Var[V] & Abort[E])): Transform[A] < S =
      Var.run(state)(using tagV) {
        Abort
          .run(using tagE) {
            v.map(a => Var.get[V](using tagV).map(finalState => (finalState, Result.Success(a))))
          }
          .map {
            case Result.Success((finalState, result)) => (finalState, result)
            case err: Result.Error[E]                 =>
              // Capture final state even on error
              Var.get[V](using tagV).map(finalState => (finalState, err))
          }
      }

    def restore[A, S](v: Transform[A] < S): A < (Var[V] & Abort[E] & S) =
      v.map { case (finalState, result) =>
        result match
          case Result.Success(a) =>
            // Success: update Var and return value
            Var.set(finalState)(using tagV) *> a

          case Result.Failure(e) if discardVarUpdatesOnFailure(e) =>
            // Restore initial state: don't update Var, propagate error
            Abort.fail(e)(using tagE)

          case Result.Failure(e) =>
            // Don't restore initial state: update Var and propagate error
            Var.set(finalState)(using tagV) *> Abort.fail(e)(using tagE)

          case Result.Panic(t) =>
            // Panic: update Var and propagate panic
            Var.set(finalState)(using tagV) *> Abort.panic(t)
      }
  end ConditionalUpdate

end VarIsolate
