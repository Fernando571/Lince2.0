package lince.backend

import lince.syntax.{Lince, Show}
import Lince.Expr
import lince.backend.Stream
import Stream.{Streams,RandomStream,RangeStream,LazyStream,ImpStream}
import scala.util.Random

object Eval:

  type Valuation = Map[String, Double]
  private type MValuation = scala.collection.Map[String, Double]

  def asBoolean(e: Expr)(using v: MValuation): Boolean =
    apply(e) match
      case b: Boolean => b
      case d: Double  => d != 0

  def asDouble(e: Expr)(using v: MValuation): Double =
    apply(e) match
      case b: Boolean => sys.error(s"Expected real, but found a boolean, at ${Show(e)}")
      case d: Double => d

  def apply(e: Expr)(using v: MValuation): Double | Boolean = e match
      case Expr.Num(n) => n
      case Expr.True => true
      case Expr.False => false
      case Expr.Var(x) => v.getOrElse(x, sys.error(s"[Eval] Variable $x not found - only ${v.keys.mkString(",")}"))

      // Arithmetic functions
      case Expr.Func("+", List(e1, e2)) => asDouble(e1) + asDouble(e2)
      case Expr.Func("-", List(e1, e2)) => asDouble(e1) - asDouble(e2)
      case Expr.Func("*", List(e1, e2)) => asDouble(e1) * asDouble(e2)
      case Expr.Func("/", List(e1, e2)) => asDouble(e1) / asDouble(e2)
      case Expr.Func("^", List(e1, e2)) => math.pow(asDouble(e1), asDouble(e2))
      case Expr.Func("pow", List(e1, e2)) => math.pow(asDouble(e1), asDouble(e2))
      case Expr.Func("sqrt", List(e)) => math.sqrt(asDouble(e))
      case Expr.Func("exp", List(e)) => math.exp(asDouble(e))
      case Expr.Func("round", List(e)) => math.round(asDouble(e)).toDouble
      case Expr.Func("sin", List(e)) => math.sin(asDouble(e))
      case Expr.Func("cos", List(e)) => math.cos(asDouble(e))
      case Expr.Func("tan", List(e)) => math.tan(asDouble(e))
      case Expr.Func("cosh", List(e)) => math.cosh(asDouble(e))
      case Expr.Func("sinh", List(e)) => math.sinh(asDouble(e))
      case Expr.Func("tanh", List(e)) => math.tanh(asDouble(e))
      case Expr.Func("arccos", List(e)) => math.acos(asDouble(e))
      case Expr.Func("arcsin", List(e)) => math.asin(asDouble(e))
      case Expr.Func("ln", List(e)) => math.log(asDouble(e))
      case Expr.Func("pi", List()) => math.Pi


      // Boolean functions
      case Expr.Func("&&", l) => l.map(asBoolean).forall(identity)
      case Expr.Func("||", l) => l.map(asBoolean).exists(identity)
      case Expr.Func("!", List(e)) => !asBoolean(e)
      case Expr.Func("==", List(e1, e2)) => apply(e1) == apply(e2)
      case Expr.Func("!=", List(e1, e2)) => apply(e1) != apply(e2)
      case Expr.Func(">=", List(e1, e2)) => asDouble(e1) >= asDouble(e2)
      case Expr.Func("<=", List(e1, e2)) => asDouble(e1) <= asDouble(e2)
      case Expr.Func(">", List(e1, e2)) => asDouble(e1) > asDouble(e2)
      case Expr.Func("<", List(e1, e2)) => asDouble(e1) < asDouble(e2)
      case Expr.Func(op, _) => sys.error(s"[Eval] Cannot evaluate function ${Show(e)}")


  /**
   * Evaluates an expression while consuming the streams
   * referenced by the expression.
   */
  def apply(e: Expr, streams: Streams)(using v: MValuation): Option[(Double | Boolean, Streams)] =
    evalStream(e, streams).map { case (evaluated, updatedStreams) =>
      (apply(evaluated), updatedStreams)
    }

  def asBoolean(e: Expr, streams: Streams)(using v: MValuation): Option[(Boolean, Streams)] =
    evalStream(e, streams).map { case (evaluated, updatedStreams) =>
      (asBoolean(evaluated), updatedStreams)
    }

  def asDouble(e: Expr, streams: Streams)(using v: MValuation): Option[(Double, Streams)] =
    evalStream(e, streams).map { case (evaluated, updatedStreams) =>
      (asDouble(evaluated), updatedStreams)
    }


  /**
   * Consumes stream values contained in an expression.
   *
   * The method is independent of the concrete Stream implementation.
   * It only relies on the Stream.pop abstraction.
   */
  def evalStream(e: Expr, streams: Streams): Option[(Expr, Streams)] = e match

      // A variable referring to a stream
      case Expr.Var(x) if streams.contains(x) => streams(x).pop match
          case Some((nextValue, nextStream)) =>
            evalStream(nextValue, streams - x).map {
              case (evaluated, updatedStreams) =>
                (evaluated, updatedStreams + (x -> nextStream))
            }

          case None => None

      // A function with no arguments referring to a stream
      case Expr.Func(name, Nil) if streams.contains(name) =>
        evalStream(Expr.Var(name), streams)


      // General function
      case Expr.Func(op, expressions) =>
        var updatedStreams = streams - op
        var failed = false
        val updatedExpressions =
          for expression <- expressions yield
            evalStream(expression, updatedStreams) match
              case None => failed = true; expression
              case Some((evaluated, newStreams)) => updatedStreams = newStreams; evaluated
        if failed then
          None
        else
          Some(Expr.Func(op, updatedExpressions) -> updatedStreams)
      // No stream to consume
      case _ => Some((e, streams))