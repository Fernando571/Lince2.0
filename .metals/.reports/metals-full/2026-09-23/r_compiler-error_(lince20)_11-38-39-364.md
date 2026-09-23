error id: A0D3C2A8B3987DC74981DD85F66D9486
file://<WORKSPACE>/src/main/scala/lince/backend/SmallStep.scala
### java.lang.StringIndexOutOfBoundsException: String index out of range: 9067

occurred in the presentation compiler.



action parameters:
offset: 9068
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

          case Some(existingStream) if existingStream.keep => Some(Action.NewStreams(name, stream) -> st.copy(
              p = Skip
            ))

          case _ => Some(Action.NewStreams(name, stream) -> st.copy(
              p = Skip,
              s = st.s + (name -> stream)
            ))


      // ------------------------------------------------------------
      // Sequential composition
      // ------------------------------------------------------------

      case Seq(Skip, q) => step(st.copy(p = q
        ))
      case Seq(p, q) =>
        for (action, st2) <- step(st.copy(p = p)
          )
        yield
          action -> st2.copy(p = Seq(st2.p, q)
            )


      // ------------------------------------------------------------
      // If-Then-Else
      // ------------------------------------------------------------

      case ITE(condition, thenProgram, elseProgram) =>

        Eval.asBoolean(condition, st.s) match
          case Some(true, updatedStreams) => Some(Action.CheckIf(condition, true) -> st.copy(
              p = thenProgram,
              s = updatedStreams
            ))

          case Some(false, updatedStreams) => Some(Action.CheckIf(condition, false) -> st.copy(
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

      case EqDiff(equations, durationExpression) =>var streams: Streams = st.s
        var stop = false

  /*
   * Evaluate each differential equation.
   *
   * The result is explicitly converted to a Map because
   * RungeKutta expects Map[String, Expr].
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
   * Evaluate the duration expression, if one exists.
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
   * Stop if one of the stream evaluations failed.
   */
  if stop then

    None

  else

    /*
     * Determine how long the differential equation
     * should be integrated.
     */
    evaluatedDuration.map(Eval.asDouble) match

      /*
       * The requested duration is greater than the
       * remaining simulation time.
       */
      case Some(duration) if duration > st.t =>

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
                Some(
                  Expr.Num(duration - st.t)
                )
              ),
              v = newValuation,
              t = 0,
              s = streams
            )
        )


      /*
       * The requested duration finishes before
       * the remaining simulation time.
       */
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


      /*
       * No duration was specified.
       * Integrate until the remaining simulation time ends.
       */
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
        )@@
```


presentation compiler configuration:
Scala version: 3.7.1-bin-nonbootstrapped
Classpath:
<WORKSPACE>/.bloop/lince20/bloop-bsp-clients-classes/classes-Metals-yxPJdVNXRuGpnU8CtlTAkQ== [exists ], <HOME>/.cache/bloop/semanticdb/com.sourcegraph.semanticdb-javac.0.12.3/semanticdb-javac-0.12.3.jar [exists ], <WORKSPACE>/.bloop/caos/bloop-bsp-clients-classes/classes-Metals-yxPJdVNXRuGpnU8CtlTAkQ== [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_sjs1_3/3.7.1/scala3-library_sjs1_3-3.7.1.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-js/scalajs-library_2.13/1.19.0/scalajs-library_2.13-1.19.0.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-parse_sjs1_3/1.1.0/cats-parse_sjs1_3-1.1.0.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/be/doeraene/scalajs-jquery_sjs1_2.13/1.0.0/scalajs-jquery_sjs1_2.13-1.0.0.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-js/scalajs-dom_sjs1_2.13/1.2.0/scalajs-dom_sjs1_2.13-1.2.0.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/scalatags_sjs1_2.13/0.9.1/scalatags_sjs1_2.13-0.9.1.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.16/scala-library-2.13.16.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-js/scalajs-javalib/1.19.0/scalajs-javalib-1.19.0.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-js/scalajs-scalalib_2.13/2.13.16%2B1.19.0/scalajs-scalalib_2.13-2.13.16%2B1.19.0.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-core_sjs1_3/2.12.0/cats-core_sjs1_3-2.12.0.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/sourcecode_sjs1_2.13/0.2.1/sourcecode_sjs1_2.13-0.2.1.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/geny_sjs1_2.13/0.6.0/geny_sjs1_2.13-0.6.0.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-kernel_sjs1_3/2.12.0/cats-kernel_sjs1_3-2.12.0.jar [exists ], <WORKSPACE>/.bloop/caos/bloop-bsp-clients-classes/classes-Metals-yxPJdVNXRuGpnU8CtlTAkQ==/META-INF/best-effort [missing ], <WORKSPACE>/.bloop/lince20/bloop-bsp-clients-classes/classes-Metals-yxPJdVNXRuGpnU8CtlTAkQ==/META-INF/best-effort [missing ]
Options:
-scalajs -Xsemanticdb -sourceroot <WORKSPACE> -Ywith-best-effort-tasty




#### Error stacktrace:

```
java.base/java.lang.StringLatin1.charAt(StringLatin1.java:48)
	java.base/java.lang.String.charAt(String.java:1517)
	scala.collection.StringOps$.apply$extension(StringOps.scala:190)
	dotty.tools.dotc.interactive.Completion$.naiveCompletionPrefix(Completion.scala:142)
	dotty.tools.dotc.interactive.Completion$.completionPrefix(Completion.scala:171)
	dotty.tools.dotc.interactive.Completion$.scopeContext(Completion.scala:57)
	dotty.tools.pc.IndexedContext$LazyWrapper.<init>(IndexedContext.scala:91)
	dotty.tools.pc.IndexedContext$.apply(IndexedContext.scala:80)
	dotty.tools.pc.AutoImportsProvider.autoImports(AutoImportsProvider.scala:48)
	dotty.tools.pc.ScalaPresentationCompiler.autoImports$$anonfun$1(ScalaPresentationCompiler.scala:317)
```
#### Short summary: 

java.lang.StringIndexOutOfBoundsException: String index out of range: 9067