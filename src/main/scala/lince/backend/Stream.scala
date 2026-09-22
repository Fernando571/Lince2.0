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
  case class RandomStream(seed: Long, retain: Boolean = true) extends Stream(retain):
    def pop = 
      val rnd = new Random(seed)
      Some(Expr.Num(rnd.nextDouble) -> RandomStream(rnd.nextLong,retain))

  /**
    * Creates a new sequential stream with the given parameters.
    * @param from the starting value
    * @param to the ending value (optional)
    * @param step the increment between values
    * @param keep whether to keep the stream in memory (default is false)
    */
  case class RangeStream(start: Double, end: Option[Double], step: Double, retain: Boolean = false) extends Stream(retain):
    def pop = if end.nonEmpty && start>end.get then None
              else Some(Expr.Num(start) -> RangeStream(start+step, end, step, retain))

  /**
    * Creates a new list stream with the given parameters.
    * @param lst the list of values
    * @param keep whether to keep the stream in memory (default is false)
    */
  case class LazyStream(lst: List[Double], retain: Boolean = false) extends Stream(retain):
    def pop = if lst.isEmpty then None
              else Some(Expr.Num(lst.head) -> LazyStream(lst.tail,retain))

  /**
    * Creates a new expression stream with the given parameters.
    * @param e the expression
    * @param keep whether to keep the stream in memory (default is false)
    */
  case class ImpStream(e:Expr, retain: Boolean = false) extends Stream(retain):
    def pop = Some(e,this)
  
