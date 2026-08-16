package lince.backend.plot

import lince.backend.plot.Plot.{MarkedPoints, Points, Traces, Trace}
import lince.backend.Eval.Valuation
import lince.syntax.Lince.PlotInfo
import scala.collection.mutable.Map as MutMap

/**
 * Builds JavaScript code to generate Plotly graphs from a given plot
 */
object PlotToTrace:

  /** Converts a plot to JavaScript instructions for Plotly */
  def apply(plot: Plot ): List[(Double,Valuation)] =
    var res: Map[Double,Valuation] =  Map()
    // res = loadTraces(plot.traces,res)
    // res = loadEndings(plot.endings,res)
    // res = loadBeginnings(plot.beginnings,res)
    // res.toList.sortBy(_._1) 
     loadTraces(plot.traces).reverse.sortBy(_._1)
  
  // private def loadTraces2(traces: Map[String,Traces]): List[(Double,Valuation)] =
  //   for (v,trs) <- traces.toList
  //       tr <- trs
  //       (t,value) <- tr yield (t,Map(v->value))

  private def loadTraces(traces: Map[String,Traces]): List[(Double,Valuation)] =
    var res = MutMap[Double,List[Valuation]]()
    for (v,trs) <- traces.toList
        tr <- trs
        (t,value) <- tr do addEvent(t,v,value)(using res)
    res.toList.flatMap{ case (t,vals) => vals.map(v => (t,v)) }
  
  // 
  private def addEvent(t: Double, v: String, value: Double)(using res: MutMap[Double,List[Valuation]]): Unit =
    res.get(t) match
      case Some(va::vals) if va.contains(v) =>
        val more = if vals.isEmpty then List(Map(v -> value))
                                   else vals.map(va2 => (va2 + (v -> value)))
        res += (t -> (va :: more)) 
      case Some(vals) =>
        res += (t -> vals.map(va2 => va2 + (v -> value)))
      case None => res += (t -> List(Map(v -> value)))
    


  // private def nextTime(traces: List[(Double,Valuation)])
  //     : (List[(Double,Valuation)], List[Double,Valuation]) =
  //   traces match
  //     case Nil => (Nil,0)
  //     case (t,v)::rest => (rest,t)

  // Needs to be adapted, to avoid overwriting values when multiple traces/points have the same timestamp.
  // private def loadTraces(traces: Map[String,Traces], acc: Map[Double,Valuation]): Map[Double,Valuation] =
  //   var res = acc
  //   for (v,trs) <- traces
  //       tr <- trs
  //       (t,value) <- tr do 
  //     res = res + (t -> (res.getOrElse(t,Map()) + (v->value)))
  //   res

  // private def loadEndings(endings: Map[String,Points], acc: Map[Double,Valuation]): Map[Double,Valuation] =
  //   var res = acc
  //   for (v,pts) <- endings
  //       (t,value) <- pts do
  //     res = res + (t -> (res.getOrElse(t,Map()) + (v->value)))
  //   res

  // private def loadBeginnings(beginnings: Map[String,MarkedPoints], acc: Map[Double,Valuation]): Map[Double,Valuation] =
  //   var res = acc
  //   for (v,mpts) <- beginnings
  //       (t,value,_) <- mpts do
  //     res = res + (t -> (res.getOrElse(t,Map()) + (v->value)))
  //   res
