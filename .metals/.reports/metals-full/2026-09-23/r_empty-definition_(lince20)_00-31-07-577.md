error id: file://<WORKSPACE>/src/main/scala/lince/backend/SmallStep.scala:lince/backend/SmallStep.St#copy().(v)
file://<WORKSPACE>/src/main/scala/lince/backend/SmallStep.scala
empty definition using pc, found symbol in pc: 
found definition using semanticdb; symbol lince/backend/SmallStep.St#v.
empty definition using fallback
non-local guesses:

offset: 8003
uri: file://<WORKSPACE>/src/main/scala/lince/backend/SmallStep.scala
text:
```scala
package lince.backend

import caos.sos.SOS
import lince.backend.Eval.Valuation
import lince.backend.SmallStep.St
import lince.backend.Stream
import Stream.{Streams,RandomStream,RangeStream,LazyStream,ImpStream}
import lince.syntax.{Lince, Show}
import lince.syntax.Lince.*
import Program.*


/**
 * Small-step semantics for commands and expressions.
 *
 * The execution state contains the current program, variable valuation,
 * active streams, simulation time and loop counter.
 */
object SmallStep extends SOS[Action, St]:

  /**
   * State of the small-step semantics.
   *
   * @param p  current program
   * @param v  current variable valuation
   * @param s  active streams
   * @param t  remaining simulation time
   * @param lp remaining loop count
   */
  case class St(
      p: Program,
      v: Valuation,
      s: Streams,
      t: Double,
      lp: Int
  )

  /**
   * Default number of Runge-Kutta samples.
   */
  val defaultRKSamples = 100


  /**
   * Initial state of the small-step semantics.
   *
   * The predefined `unif` stream is initialized using the
   * seed specified in the simulation parameters.
   *
   * @param si simulation parsed from the user's input
   */
  def initial(si: Simulation) =
    St(si.prog,Map(),
      Map("unif" -> RandomStream(si.pi.seed)),
      si.pi.maxTime,si.pi.maxLoops)


  /**
   * Determines whether a state is accepting.
   *
   * A simulation is considered complete when either the
   * available simulation time or the maximum number of loops
   * reaches zero.
   */
  override def accepting(s: St): Boolean =
    s.t <= 0 || s.lp <= 0


  /**
   * Returns the set of possible evolutions from a state.
   *
   * The current semantics are deterministic, therefore the
   * result contains at most one transition.
   */
  def next[A >: Action](st: St): Set[(A, St)] =
    step(st)(using defaultRKSamples).toSet


  /**
   * Performs one deterministic small step.
   */
  def step(st: St)(using rkSamples: Int): Option[(Action, St)] =

    /*
     * Stop the execution when the simulation time or
     * maximum number of loops has been exhausted.
     */
    if st.t <= 0 || st.lp <= 0 then
      return None

    given v: Valuation = st.v
    st.p match


      // ------------------------------------------------------------
      // Skip
      // ------------------------------------------------------------

      case Skip => None


      // ------------------------------------------------------------
      // Variable assignment
      // ------------------------------------------------------------
      case Assign(name, expression) =>
        /*
         * A variable cannot override an existing stream.
         */
        if st.s contains name then sys.error(s"Variable definition ${Show(st.p)} " + s"overriding an existing stream.")
        /*
         * Evaluate the expression and update the stream state.
         */
        val result = Eval.asDouble(expression, st.s)
        result.map { case (value, updatedStreams) =>
          Action.Assign(name, value) -> st.copy(p = Skip, v = v + (name -> value), s = updatedStreams)
        }

      // ------------------------------------------------------------
      // Stream definition
      // ------------------------------------------------------------

      case StrAssign(name, stream) =>
        /*
         * A stream cannot override an existing variable.
         */
        if st.v contains name then sys.error(s"Stream definition ${Show(st.p)} " + s"overriding an existing variable.")
        /*
         * If an existing stream is marked as retained,
         * preserve the existing stream.
         *
         * Otherwise replace it with the new stream.
         */
        st.s.get(name) match

          case Some(existingStream) if existingStream.retain =>

            Some(Action.NewStreams(name, stream) -> st.copy(
              p = Skip
            ))

          case _ =>

            Some(Action.NewStreams(name, stream) -> st.copy(
              p = Skip,
              s = st.s + (name -> stream)
            ))


      // ------------------------------------------------------------
      // Sequential composition
      // ------------------------------------------------------------

      case Seq(Skip, q) => step(st.copy(
          p = q
        ))


      case Seq(p, q) =>
        for (action, st2) <- step(
            st.copy(p = p)
          )
        yield
          action -> st2.copy(
              p = Seq(st2.p, q)
            )


      // ------------------------------------------------------------
      // If-Then-Else
      // ------------------------------------------------------------

      case ITE(condition, thenProgram, elseProgram) =>

        Eval.asBoolean(condition, st.s) match
          case Some(true, updatedStreams) =>
            Some(Action.CheckIf(condition, true) -> st.copy(
              p = thenProgram,
              s = updatedStreams
            ))

          case Some(false, updatedStreams) =>
            Some(Action.CheckIf(condition, false) -> st.copy(
              p = elseProgram,
              s = updatedStreams
            ))
          case None => None


      // ------------------------------------------------------------
      // While
      // ------------------------------------------------------------

      case wh @ While(condition, body) =>

        Eval.asBoolean(condition, st.s) match
          case Some(true, updatedStreams) =>
            Some(Action.CheckWhile(condition, true) -> st.copy(
              p = Seq(body, wh),
              lp = st.lp - 1,
              s = updatedStreams
            ))
          case Some(false, updatedStreams) =>

            Some(Action.CheckWhile(condition, false) -> st.copy(
              p = Skip,
              s = updatedStreams
            ))
          case None => None


      // ------------------------------------------------------------
      // Differential equations
      // ------------------------------------------------------------

      case EqDiff(equations, durationExpression) =>

        /*
         * The expressions defining the differential equations
         * may contain streams. Therefore, evaluate them first
         * and update the stream state.
         */
        var streams = st.s
        var stop = false

        /*
         * Evaluate the equations.
         */
        val evaluatedEquations =

          for (variable, expression) <- equations
          yield
            Eval.evalStream(expression, streams) match
              case None => stop = true
                (variable, expression)
              case Some((evaluatedExpression, updatedStreams)) =>
                streams = updatedStreams
                (variable, evaluatedExpression)

        /*
         * Evaluate the duration expression, if one exists.
         */
        val evaluatedDuration = durationExpression.map { duration =>
            Eval.evalStream(duration, streams) match
              case None => stop = true
                duration
              case Some((evaluatedExpression, updatedStreams)) => streams = updatedStreams
                evaluatedExpression
          }

        /*
         * Stop if one of the stream evaluations failed.
         */
        if stop then None

        else
          /*
           * Determine how long the differential equation
           * should be integrated.
           */
          evaluatedDuration.map(Eval.asDouble) match

            // ------------------------------------------------------
            // Duration is greater than the remaining simulation time
            // ------------------------------------------------------

            case Some(duration) if duration > st.t => val newValuation = RungeKutta(v, evaluatedEquations, st.t, rkSamples)
              Some(Action.DiffStop(evaluatedEquations, st.t) -> st.copy(
                    p = EqDiff(evaluatedEquations,
                      Some(Expr.Num(duration - st.t))
                    ), @@v = newValuation, t = 0, s = streams
                  ))

            // ------------------------------------------------------
            // Duration finishes before the simulation time
            // ------------------------------------------------------

            case Some(duration) =>

              val newValuation =
                RungeKutta(
                  v,
                  evaluatedEquations,
                  duration,
                  rkSamples
                )

              Some(

                Action.DiffSkip(
                  evaluatedEquations,
                  duration
                ) ->

                  st.copy(
                    p = Skip,
                    v = newValuation,
                    t = st.t - duration,
                    s = streams
                  )
              )


            // ------------------------------------------------------
            // No duration: integrate until simulation time ends
            // ------------------------------------------------------

            case None =>

              val newValuation =
                RungeKutta(
                  v,
                  evaluatedEquations,
                  st.t,
                  rkSamples
                )

              Some(

                Action.DiffStop(
                  evaluatedEquations,
                  st.t
                ) ->

                  st.copy(
                    p = EqDiff(
                      evaluatedEquations,
                      None
                    ),
                    v = newValuation,
                    t = 0,
                    s = streams
                  )
              )
```


#### Short summary: 

empty definition using pc, found symbol in pc: 