package lince.backend

import lince.syntax.Lince.* 
import java.util.Random
import Stream.*

/**
 * Represents an immutable source of values used during simulation.
 *
 * A Stream produces values on demand. Each call to `pop` returns
 * the next value together with the updated stream state.
 */
sealed trait Stream(val keep: Boolean = false):

  def pop: Option[(Expr, Stream)]


object Stream:

  type Streams = Map[String, Stream]


  /**
   * Pseudo-random stream. Escoamento pseudoaleatório. Cada chamada produz um valor pseudo-aleatório e um novo fluxo, contendo o estado atualizado do gerador.
   *
   * Each call produces a pseudo-random value and a new stream
   * containing the updated generator state.
   */
  case class RandomStream(seed: Long, kp: Boolean = true) extends Stream(kp):

    def pop: Option[(Expr, Stream)] =
      val generator = new Random(seed)
      val value = generator.nextDouble()
      val nextSeed = generator.nextLong()

      Some(Expr.Num(value) -> RandomStream(nextSeed, kp))


  /**
   * Deterministic numerical sequence.
   *
   * Example:
   * RangeStream(1, Some(10), 2)
   *
   * produces:
   * 1, 3, 5, 7, 9
   */
  case class RangeStream(start: Double, end: Option[Double], step: Double, kp: Boolean = false) extends Stream(kp):

    require(step != 0, "RangeStream step cannot be zero")

    def pop: Option[(Expr, Stream)] =

      val finished = end match
        case Some(limit) => if step > 0 then start > limit else if step < 0 then start < limit else true
        case None => false

      if finished then None
      else
        Some(Expr.Num(start) -> RangeStream(start + step, end, step, kp))



  /**
   * Stream backed by a finite sequence of values. Stream de dados suportado por uma sequência finita de valores.
   */
  case class LazyStream(
      values: List[Double],
      kp: Boolean = false
  ) extends Stream(kp):

    def pop: Option[(Expr, Stream)] =
      values match
        case Nil =>
          None

        case head :: tail =>
          Some(
            Expr.Num(head) ->
              LazyStream(tail, kp)
          )


  /**
   * Stream whose values are defined by an expression. Stream cujos valores são definidos por uma expressão.
   */
  case class ImpStream(
      expression: Expr,
      kp: Boolean = false
  ) extends Stream(kp):

    def pop: Option[(Expr, Stream)] =
      Some(expression -> this)