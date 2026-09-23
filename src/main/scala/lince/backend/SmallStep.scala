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
     * Stop the execution when the simulation time or  **Interrompa a execução quando o tempo de simulação ou o número máximo de iterações for atingido.
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
         * A variable cannot override an existing stream. *Uma variável não pode sobrescrever um stream existente.
         */
        if st.s contains name then sys.error(s"Variable definition ${Show(st.p)} " + s"overriding an existing stream.")
        /*
         * Evaluate the expression and update the stream state. *Avalie a expressão e atualize o estado do stream.
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
         * A stream cannot override an existing variable. ** Um fluxo não pode sobrescrever uma variável existente.
         */
        if st.v contains name then sys.error(s"Stream definition ${Show(st.p)} " + s"overriding an existing variable.")
        /*
         * If an existing stream is marked as retained, Se um fluxo existente estiver marcado como mantido, preserve o stream existente. Caso contrário, substitua-o pelo novo stream.
         * preserve the existing stream.
         *
         * Otherwise replace it with the new stream.
         */
        st.s.get(name) match

          case Some(existingStream) if existingStream.keep => Some(Action.NewStreams(name, stream) -> st.copy(
              p = Skip))
          case _ => Some(Action.NewStreams(name, stream) -> st.copy(
              p = Skip,
              s = st.s + (name -> stream)))


      // ------------------------------------------------------------
      // Sequential composition
      // ------------------------------------------------------------

      case Seq(Skip, q) => step(st.copy(p = q))
      case Seq(p, q) =>
        for (action, st2) <- step(st.copy(p = p))
        yield
          action -> st2.copy(p = Seq(st2.p, q))


      // ------------------------------------------------------------
      // If-Then-Else
      // ------------------------------------------------------------

      case ITE(condition, thenProgram, elseProgram) =>
        Eval.asBoolean(condition, st.s) match
          case Some(true, updatedStreams) => Some(Action.CheckIf(condition, true) -> st.copy(
              p = thenProgram,
              s = updatedStreams))
          case Some(false, updatedStreams) => Some(Action.CheckIf(condition, false) -> st.copy(
              p = elseProgram,
              s = updatedStreams))
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
         * Evaluate the expressions defining the differential equations. * Avalie as expressões que definem as equações diferenciais. Os valores do fluxo são consumidos durante este processo e o estado do fluxo atualizado é preservado.
         * Stream values are consumed during this process and the updated
         * stream state is preserved.
         */
        var streams: Streams = st.s
        var stop = false

        /*
         * Evaluate each differential equation. Avalie cada equação diferencial. O método de Runge-Kutta espera um Map[String, Expr], pelo que o resultado é explicitamente digitado como Map[String, Expr].
         *
         * RungeKutta expects Map[String, Expr], therefore the result
         * is explicitly typed as Map[String, Expr].
         */
        val evaluatedEquations: Map[String, Expr] = equations.map { case (variable, expression) =>
            Eval.evalStream(expression, streams) match
              case None =>
                stop = true
                variable -> expression
              case Some((evaluatedExpression, updatedStreams)) =>
                streams = updatedStreams
                variable -> evaluatedExpression
          }

        /*
         * Evaluate the duration expression, if one exists. Avalie a expressão de duração, se existir.
         */
        val evaluatedDuration: Option[Expr] =
          durationExpression.map { duration =>

            Eval.evalStream(duration, streams) match
              case None =>
                stop = true
                duration
              case Some((evaluatedExpression, updatedStreams)) =>
                streams = updatedStreams
                evaluatedExpression
          }

        /*
         * Stop if one of the stream evaluations failed. Interrompa o processo se uma das avaliações do fluxo falhar.
         */
        if stop then
          None

        else
          /*
           * Determine how long the differential equation. Determine durante quanto tempo a equação diferencial deve ser integrada.
           * should be integrated.
           */
          evaluatedDuration.map(Eval.asDouble) match

            /*
             * The requested duration is greater than the
             * remaining simulation time. A duração solicitada é superior ao tempo restante da simulação.
             */
            case Some(duration) if duration > st.t =>
              val newValuation = RungeKutta(v, evaluatedEquations, st.t, rkSamples)
              Some(Action.DiffStop(evaluatedEquations, st.t) -> st.copy(p = 
                EqDiff(evaluatedEquations,
                      Some(Expr.Num(duration - st.t))),
                    v = newValuation,
                    t = 0,
                    s = streams
                  ))

            /*
             * The requested duration fits inside. A duração solicitada está dentro do tempo restante da simulação.
             * the remaining simulation time.
             */
            case Some(duration) =>
              val newValuation = RungeKutta(v, evaluatedEquations, duration, rkSamples)
              Some(Action.DiffSkip(evaluatedEquations, duration) ->
                  st.copy(
                    p = Skip,
                    v = newValuation,
                    t = st.t - duration,
                    s = streams
                  ))

            /*
             * No duration was specified.
             * Integrate until the end of the simulation. Nenhuma duração foi especificada. Integre até ao final da simulação.
             */
            case None =>
              val newValuation = RungeKutta(
                  v, evaluatedEquations, st.t, rkSamples)
              Some(Action.DiffStop(
                  evaluatedEquations, st.t) -> st.copy(
                    p = EqDiff(evaluatedEquations,
                      None),
                    v = newValuation,
                    t = 0,
                    s = streams
                  ))