package lince.backend

import caos.sos.SOS
import lince.backend.Eval.Valuation
import lince.backend.SmallStep.St
import lince.backend.Stream
import Stream.{Streams,RandomStrm,SeqStrm,ListStrm,ExprStrm}
import lince.syntax.{Lince, Show}
import lince.syntax.Lince.*
import Program.*

import scala.annotation.tailrec
import scala.util.Random

/** Small-step semantics for both commands and boolean+integer expressions.  */
object SmallStep extends SOS[Action,St]:

  case class St(p: Program   // input program
               ,v: Valuation // known variables
               ,s: Streams   // known streams
               ,t: Double    // maximum time
               ,lp:Int)     // maximum loops

  val defaultRKSamples = 100

  /**
    * Initial state of the small-step semantics
    *
    * @param si simulation parsed from the user's input
    */
  def initial(si: Simulation) =
    St(si.prog,Map(),
       Map("unif" -> RandomStrm((si.pi.seed))), //+(si.pi.runs-1)))),
       si.pi.maxTime,si.pi.maxLoops)

  // // streams in the state
  // type Strms = Map[String,Strm]
  // case class RandomStrm(seed: Long, kp: Boolean = true) extends Strm(kp):
  //   def pop = 
  //     val rnd = new Random(seed)
  //     Some(Expr.Num(rnd.nextDouble) -> RandomStrm(rnd.nextLong,keep))
  // case class SeqStrm(from: Double, to: Option[Double], step: Double, kp: Boolean) extends Strm(kp):
  //   def pop = if to.nonEmpty && from>to.get then None
  //             else Some(Expr.Num(from) -> SeqStrm(from+step, to, step, keep))
  // case class ListStrm(lst: List[Double], kp: Boolean) extends Strm(kp):
  //   def pop = if lst.isEmpty then None
  //             else Some(Expr.Num(lst.head) -> ListStrm(lst.tail,keep))
  // case class ExprStrm(e:Expr, kp: Boolean) extends Strm(kp):
  //   def pop = Some(e,this)

  override def accepting(s: St): Boolean =
    s.t<=0 || s.lp<=0

  /** What are the set of possible evolutions (label and new state) */
  def next[A>:Action](st: St): Set[(A, St)] =
    step(st)(using defaultRKSamples).toSet

  /** Performs a single (deterministic) small step */
  def step(st: St)(using rkSamples: Int): Option[(Action, St)] =
    if st.t<=0 || st.lp<=0 then
      return None

    given v:Valuation = st.v

    st.p match {
      case Skip => None
      case Assign(n, e) =>
        if st.s contains n then sys.error(s"Variable definition ${Show(st.p)} overriding an existient stream.")
        val ress = Eval.asDouble(e,st.s) // after Eval always update the seed of the state
        // println(s"#### evaluating ${Show(e)} --> got ${ress} (from ${Show.simpleSt(st)})")
        // Some(Action.Assign(n,res) ->  st.nextSeed.copy(p = Skip, v = v+(n->res)))
        ress.map((res,ss) =>
          Action.Assign(n,res) ->  st.copy(p = Skip, v = v+(n->res), s = ss))
      case StreamDef(n, s) =>
        if st.v contains n then sys.error(s"Stream definition ${Show(st.p)} overriding an existient variable.")
        st.s.get(n) match
          case Some(strm) if strm.keep =>
                    Some(Action.StrmDef(n,s) -> st.copy(p=Skip))
          case _ => Some(Action.StrmDef(n,s) -> st.copy(p=Skip,s=st.s+(n->s)))
      case Seq(Skip, q) => step(st.copy(p=q))
      case Seq(p, q) =>
        for (a,st2) <- step(st.copy(p=p))
          yield a -> st2.copy(p=Seq(st2.p,q))
      case ITE(b, pt, pf) =>
        Eval.asBoolean(b,st.s) match
          case Some(true,ss)  => Some(Action.CheckIf(b,true)  -> st.copy(p=pt,s=ss))
          case Some(false,ss) => Some(Action.CheckIf(b,false) -> st.copy(p=pf,s=ss))
          case None => None
      case wh@While(b, p) =>
        Eval.asBoolean(b,st.s) match 
          case Some(true,ss)  => Some(Action.CheckWhile(b,true)  -> st.copy(p=Seq(p,wh), lp=st.lp-1, s=ss))
          case Some(false,ss) => Some(Action.CheckWhile(b,false) -> st.copy(p=Skip, s=ss))
          case None => None
      case EqDiff(eqs, durExp) =>
        // evaluate and update streams
        var ss = st.s
        var stop = false
        val eqs2 = for (v,e) <- eqs yield
          Eval.evalStreams(e,ss) match
            case None => {stop = true; (v,e)}
            case Some((e2,ss2)) => {ss = ss2; (v,e2)}
        val durExp2 = durExp.map(d =>
          Eval.evalStreams(d,ss) match
            case None => {stop = true; d}
            case Some((d2,ss2)) => {ss = ss2; d2}
        )

        // apply RungeKutta until st.t or durExp
        if stop then None else        
          durExp2.map(Eval.asDouble) match
            case Some(dur) if dur>st.t => // time to stop is before the duration
              val v2 = RungeKutta(v,eqs2,st.t,rkSamples)
              Some(Action.DiffStop(eqs2,st.t) ->
                    st.copy(p=EqDiff(eqs2,Some(Expr.Num(dur-st.t))), v=v2, t=0, s=ss))
            case Some(dur) => // time to stop is after the duration
              val v2 = RungeKutta(v,eqs2,dur,rkSamples)
              Some(Action.DiffSkip(eqs2,dur) ->
                    st.copy(p=Skip, v=v2, t=st.t-dur, s=ss))
            case None => // the time to stop is before the (infinite) duration
              val v2 = RungeKutta(v,eqs2,st.t,rkSamples)
              Some(Action.DiffStop(eqs2,st.t) ->
                    st.copy(p=EqDiff(eqs2,None), v=v2, t=0, s=ss))
    }

