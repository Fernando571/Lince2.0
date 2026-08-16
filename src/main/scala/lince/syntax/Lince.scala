package lince.syntax

import lince.backend.Stream

/**
 * Internal structure to represent terms in Lince 2.0.
 */

object Lince:

  ///// Program ////

  enum Program:
    case Skip
    case Assign(v:String, e:Expr)
    case StreamDef(v:String, s:Stream)
    case EqDiff(eqs:Map[String,Expr], dur:Option[Expr])
    case Seq(p:Program, q:Program)
    case ITE(b:Expr, pt:Program, pf:Program)
    case While(b:Expr, p:Program)

  enum Expr:
    case Num(n:Double)
    case True
    case False
    case Var(x:String)
    case Func(op:String, es:List[Expr])

  ///// Actions ////

  enum Action:
    case Assign(v: String, n:Double)
    case StrmDef(v: String, s:Stream)
    case DiffStop(eqs: Map[String, Expr], time: Double)
    case DiffSkip(eqs: Map[String, Expr], time: Double)
    case CheckIf(b: Expr, res:Boolean)
    case CheckWhile(b: Expr, res:Boolean)
    override def toString: String = Show(this)

  ///// Plot configuration ////

  case class Simulation(prog:Program, pi:PlotInfo)

  case class PlotInfo( minTime:Double,
                       maxTime:Double,
                       maxLoops: Int,
                       samples: Int,
                       rkSamples: Int,
                       seed: Long,
                       showAll: Boolean,
                       showVar: String=>Boolean,
                       height: Int, // height in px
                       runs: Int, // number of times to repeat the run
                       portrait: List[(String,String)], // to change the variables in the x and y axis
                       monSampleFreq: Double, // frequency of sampling for the monitor (in time units)
                       monSampleNoise: Double, // noise to add to the monitor sampling time (e.g., 0.1 means that sampling time is uniformly distributed in [t-0.1, t+0.1])
  )
  object PlotInfo:
    def default = PlotInfo(0,10,500,40,100,
      (new scala.util.Random).nextLong(),
      false,_=>true,450,1,Nil,1,0)


