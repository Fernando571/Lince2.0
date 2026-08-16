package lince.backend

import lince.backend.Eval.Valuation
import lince.backend.SmallStep.{St, step}
import lince.syntax.Lince.*
import lince.syntax.Lince.Program.*
import lince.syntax.Show

import scala.annotation.tailrec

/**
 * Collection of functions that perform more than one small step at a time.
 * This includes just discrete steps, just continuous steps, or until the end.
 */

object BigSteps:

  /** Performs all steps until no more step can be taken. */
  def bigStep(st: St, hist: List[Action] = Nil)(using rkSamples: Int): (List[Action], St) =
    step(st)(using rkSamples) match
      case Some((act, st2)) => bigStep(st2, act :: hist)
      case None => hist -> st

  /**
   * Checks the next statement in a sequence of statements, and jumping over Skips.
   * @param p Program to inspect
   * @return the next statement of the program `p`
   */
  def nextStatement(p: Program): Program = p match {
    case Seq(Seq(p1, p2), q) =>
      nextStatement(Seq(p1, Seq(p2, q)))
    case Seq(Skip, q) => nextStatement(q)
    case Seq(p, q) => nextStatement(p)
    case _ => p
  }

  def nextStatementRest(p: Program, rest:Program = Skip): (Program,Program) = p match {
    case Seq(Seq(p1, p2), q) =>
      nextStatementRest(Seq(p1, Seq(p2, q)), rest)
    case Seq(Skip, q) => nextStatementRest(q,rest)
    case Seq(p, q) => nextStatementRest(p,Seq(q,rest))
    case _ => (p,rest)
  }
  
  /**
   * Performs discrete steps until no more step can be taken
   * @param st initial state
   * @param hist history of actions taken
   * @return Pair with the list of actions taken and the reached state
   */
  @tailrec
  def discSteps(st: St, hist: List[Action] = Nil)(using rkSamples: Int): (List[Action], St) =
    nextStatement(st.p) match
      case _:EqDiff => hist -> st
      case _ => step(st) match
        case None => hist -> st // reached the end
        case Some((a, st2)) => discSteps(st2, a :: hist)


  /**
   * Traverses a state of the small-step semantics iteratively just considering continuous steps, until either:
   *   - no more continuous step can be taken, or
   *   - the time bound is reached.
   * Compiles a sequence of valuations along the path, sampled every `timeStep`.
   * @param st state to be traversed
   * @param timeStep time interval used to sample values while traversing the state
   * @param baseTime time passed at the beginning of the traversal (to be added)
   * @param counter accumulator to know how many values were sampled
   * @param hist accumulator to compile the valuations of points already sampled
   * @return list of valuations at the points sampled while traversing the continuous step
   */
  def contSteps(st: St, timeStep: Double, baseTime:Double)(using rkSamples: Int): (List[(Double,Valuation)], St) =
    // need to evaluate the duration upfront, in case there are random functions.
//    var ok = true
//    val newP: Program = nextStatementRest(st.p) match
//      case (EqDiff(eqs,dur),rest) =>
//        Program.Seq(EqDiff(eqs,Expr.Num(Eval(dur)(using st.v, st.r))),rest)
//      case p2 =>
//        ok = false
//        st.p
//    if !ok then return Nil -> st
    ////////st.resetSeed

    // Alternative (maybe preferred in the future):
    //  1. extract the diff-eqs (stop if not found) and extract the rest
    //  2. pre-process the diff-eqs (replace stream-/random-functions with their value)
    //  3. use auxiliar function on the single program
    //  4. re-attach the result or just keep the rest, and update the state

    // now apply the recursive steps after fixing the duration
    @tailrec
    def contStepsAux(counter:Int,
                     hist: List[(Double,Valuation)])(using rkSamples: Int): (List[(Double,Valuation)], St) =
      val goalTime = st.t min (timeStep*counter)
  //    println(s"-- contSteps ${st} with goal $goalTime and next ${nextStatement(st.p)}")
      nextStatement(st.p) match
        case EqDiff(eqs,dur) => step(st.copy(t = goalTime)) match
          case Some((Action.DiffStop(_,_),st2)) => // reached goalTime
            //            println(s"[CS] Diff-stop - reached the goal time (min t/ts*counter)\n   ${(baseTime+goalTime::hist) -> st2}")
            if goalTime == st.t // if it stopped because of the boundaries, then stop, otherwise keep on going
            then
              val st3 = st2 //////.nextSeed // update seed only at the end
              (((baseTime+goalTime)->st3.v)::hist) -> st3
            else contStepsAux(counter+1, ((baseTime+goalTime) -> st2.v)::hist)
          case Some((Action.DiffSkip(_,timePassed),st2)) => // "diff-skip // reached duration
            //            println(s"[CS] reached duration\n    FROM ${Show.simpleSt(st)}\n    BY $a\n    TO ${Show.simpleSt(st2)}")
            val st3 = st2 ////////.nextSeed // update seed only at the end
            (((baseTime+timePassed)->st3.v)::hist) -> st3.copy(t = st.t-timePassed)
          case Some((stp,_)) => sys.error(s"Expected continuous step but found ${Show(stp)}")
          case None => hist -> st
        //            println(s"[CS] no step possible using time ${st.t} MIN ${timeStep*counter}");

        case _ => Nil -> st
    //
    contStepsAux(1, Nil)

