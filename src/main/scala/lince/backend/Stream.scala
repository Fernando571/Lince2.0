package lince.backend

import lince.syntax.Lince.* 
import java.util.Random
import Stream.*

/**
  * Represents an immutable stream of values.
  * A stream is a potentially infinite sequence of values that can be generated on demand. The `pop` method returns the next value in the stream along with the updated stream.
  */
sealed trait Stream(val keep: Boolean = false):
  /**
    * Returns the next value in the stream along with the updated stream.
    * @return an option containing a tuple of the next value and the updated stream, or None if the stream is empty.
    */
  def pop: Option[(Expr,Stream)]
  

object Stream:
  
  // A collection of streams to be used in the small-step semantics. Each stream is identified by a string key.
  type Streams = Map[String,Stream]

  /**
    * Creates a new random stream with the given seed and keep flag.
    * @param seed the seed for the random number generator
    * @param keep whether to keep the stream in memory (default is true)
    */
  case class RandomStrm(seed: Long, kp: Boolean = true) extends Stream(kp):
    def pop = 
      val rnd = new Random(seed)
      Some(Expr.Num(rnd.nextDouble) -> RandomStrm(rnd.nextLong,kp))

  /**
    * Creates a new sequential stream with the given parameters.
    * @param from the starting value
    * @param to the ending value (optional)
    * @param step the increment between values
    * @param keep whether to keep the stream in memory (default is false)
    */
  case class SeqStrm(from: Double, to: Option[Double], step: Double, kp: Boolean = false) extends Stream(kp):
    def pop = if to.nonEmpty && from>to.get then None
              else Some(Expr.Num(from) -> SeqStrm(from+step, to, step, kp))

  /**
    * Creates a new list stream with the given parameters.
    * @param lst the list of values
    * @param keep whether to keep the stream in memory (default is false)
    */
  case class ListStrm(lst: List[Double], kp: Boolean = false) extends Stream(kp):
    def pop = if lst.isEmpty then None
              else Some(Expr.Num(lst.head) -> ListStrm(lst.tail,kp))

  /**
    * Creates a new expression stream with the given parameters.
    * @param e the expression
    * @param keep whether to keep the stream in memory (default is false)
    */
  case class ExprStrm(e:Expr, kp: Boolean = false) extends Stream(kp):
    def pop = Some(e,this)
