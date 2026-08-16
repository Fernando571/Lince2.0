package lince.backend.plot

import lince.backend.BigSteps.{contSteps, discSteps}
import lince.backend.plot.Plot.{MarkedPoints, Points, Trace, Traces}
import lince.backend.{BigSteps, SmallStep}
import lince.syntax.Lince
import lince.syntax.Lince.{Action, Expr, PlotInfo, Program, Simulation}

import scala.annotation.tailrec


/**
 * Structure to compile the data that is in a plot
 * @param current maps the current trace (a segment in the plot) to each variable
 * @param traces maps the collection of finished traces to each variable
 * @param endings maps the ending of traces to each variable
 * @param beginnings maps the beginning of traces to each variable
 */
case class Plot(current: Map[String,Trace], traces: Map[String,Traces],
                endings: Map[String,Points], beginnings: Map[String,MarkedPoints],
                xlabels: Set[String], ylabels:Set[String]):
  /** Adds a new point to the current plot. Usage: `plot + ("x" -> now -> value)` */
  def +(varTimeValue:((String,Double),Double)): Plot =
    val ((x,time),value) = varTimeValue
    val oldTr: Trace = current.getOrElse(x,Nil)
    this.copy(current = current + (x -> ((time -> value) :: oldTr)))

  /** Ends the current trace, adding the last point to the ending markers. */
  private def endTrace(x:String): Plot =
    current.getOrElse(x,Nil).headOption match
      case Some(lastValue) =>
        this.copy(current = current - x, //+ (x -> Nil),
                  traces  = traces +  (x -> (current.getOrElse(x,Nil)::traces.getOrElse(x,Nil))),
                  endings = endings + (x -> (lastValue::endings.getOrElse(x,Nil))))
      case None => this

  @tailrec
  final def endTraces: Plot =
    current.headOption match {
      case Some((v,trace)) => endTrace(v).endTraces
      case None => this
    }

  /** Starts a new trace, ending a possible previous one, adding the beginning and ending markers.  */
  private def startTrace(x: String, time: Double, value: Double, act:List[Action]): Plot =
    endTrace(x)
      .copy(beginnings = beginnings + (x -> ((time,value,act)::beginnings.getOrElse(x,Nil))),
            current = current + (x -> List(time->value)))

  def show: String =
    (for x <- (current.keys ++ traces.keys ++ endings.keys ++ beginnings.keys) yield
      s"$x:\n    cur: ${
        current.getOrElse(x,Nil).mkString(",")}\n    trc: ${
        traces.getOrElse(x,Nil).map(x => x.mkString(",")).mkString("\n         ")}\n    end: ${
        endings.getOrElse(x,Nil).mkString(",")}\n    begin: ${
        beginnings.getOrElse(x,Nil).mkString(",")}\nlabels (x|y): ${
        xlabels.mkString("/")}|${ylabels.mkString("/")}")
      .mkString("\n")


object Plot:

  type Traces = List[Trace]
  type Trace  = List[(Double,Double)]
  type Points = List[(Double,Double)]
  type MarkedPoints = List[(Double,Double,List[Action])]
  def empty = Plot(Map(),Map(),Map(),Map(),Set("time"),Set())

  private type St = SmallStep.St

  def allPlots(st:St, pinfo:PlotInfo): List[(Plot,PlotInfo)] =
    var lastSt = st
    val ps = for run <- (1 to pinfo.runs).toList yield
      val pi2 = pinfo.copy(runs = run)
      val initState = SmallStep.initial(Simulation(st.p,pi2)).copy(s = lastSt.s)
      //println(s"[RUN $run] ${initState.s}")
      apply(initState, pi2).map((p,stRun) =>
          lastSt = stRun
          (p,pi2)
        )
    ps.flatten
//      (apply(Simulation(st.p,pi2).state, pi2),pi2)

  def justPlot(st:St, pinfo:PlotInfo): List[Plot] =
    apply(st,pinfo).map(_._1)

  def apply(st:St, pinfo:PlotInfo): List[(Plot,St)] =
    val (plot,st2) = apply(st, pinfo.minTime, pinfo.maxTime,
                           pinfo.samples, pinfo.rkSamples, pinfo.showAll, pinfo.showVar)
    // transform it into a portrait plot if needed
    if pinfo.portrait.nonEmpty
    then rearrange(plot,pinfo.portrait).map(_ -> st2)
    else List(plot->st2)

  /**
   *  Calculate a plot by traversing the state-space while collecting points and action names.
   * @param st initial state
   * @param from starting time
   * @param to ending time
   * @param samples number of time points to sample (other than the starting point),
   * @param showCont true if the borders of all subtrajectories should be marked, instead of just when there is an assignment between subtrajectories
   * @param filter given a variable name, returns true if the variable should be included in the plot
   * @return the Plot containing the points and action names while traversing a hybrid program
   */
  def apply(st: St, from: Double, to: Double,
            samples:Int=50, rkSamples: Int=100,
            showCont:Boolean=false,filter:String=>Boolean): (Plot,St) = {

    // need to traverse my trajectory
    // need a maxt
    val maxt: Double = to min st.t
    // need a step size
    val stepSize: Double = (maxt - from) / samples

    val stInit = if from!=0
      then valToAssign(BigSteps.bigStep(st.copy(t=from))(using rkSamples)._2.copy(t=maxt-from))
      else st.copy(t = maxt)

//    val stInit = st.copy(t = maxt) // need to start after navigating to time mint!
                                 // need bigstep to mint.
//    apply(st, stepSize, mint, "")
    val (plot,st2) = calcPlot(stInit, stepSize, rkSamples, from, showCont, Plot.empty, filter)
    plot.endTraces -> st2
  }

  /** Converts the state of a program (given by the values of the variables) into an introductory sequence of assignments. */
  private def valToAssign(st:St): St =
    val assign = for (x,value) <- st.v yield Program.Assign(x,Expr.Num(value))
    st.copy(p = assign.foldRight(st.p)(Program.Seq.apply), v = Map())

  /**
   * Main function that produces the plot: at each run performs a collection of
   * discrete and a collection of continuous steps, untin the end or bound is reached.
   * @param st initial state
   * @param stepSize time interval to sample
   * @param timePassed time that passed at the beginning
   * @param acc accumulated plot
   * @return plot from the run
   */
  @tailrec
  def calcPlot(st: St, stepSize: Double, rkSamples: Int, timePassed: Double, showCont:Boolean, acc: Plot, filter:String=>Boolean): (Plot,St) =
    var res = acc
    // run discrete steps
    val (as, st2) = discSteps(st)(using rkSamples)
    // update Plot
    val setVars = if showCont
      then st2.v.keySet
      else for (case Action.Assign(v,_) <- as.toSet) yield v
    for (v <- setVars if filter(v)) do
      res = res.startTrace(v,
        timePassed,
        st2.v.getOrElse(v,sys.error(s"No value for ${v} after ${as.mkString(",")}")),
        as
      ).copy(ylabels = res.ylabels+v)

    // run continuous steps while sampling
    val (points, st3) = contSteps(st2, stepSize, timePassed)(using rkSamples)
    // update Plot
    for ((time,valuation) <- points.reverse; (x,value) <- valuation if filter(x)) do
      res = res + (x -> time -> value)

    if SmallStep.accepting(st3) || st == st3 then  res -> st2 // res + "## Finished"
    else calcPlot(st3, stepSize, rkSamples, timePassed + (st2.t - st3.t), showCont, res, filter)


  def rearrange(p:Plot, axis:List[(String,String)]): List[Plot] =
    axis.map(ax => rearrange(p,ax))

  /**
   * Change the values being plotted in the x and y axis
   * @param p original plot
   * @param axis pair of variable names to be used
   * @return updated plot
   */
  def rearrange(p:Plot, axis:(String,String)): Plot =
    val (x,y) = axis
    val trX = p.traces.getOrElse(x,Nil)
    val trY = p.traces.getOrElse(y,Nil)
    val newTr   = mergeTr(trX,trY)
    val newBgs  = mergeBgs(p.beginnings.getOrElse(x,Nil),
                           p.beginnings.getOrElse(y,Nil))
    val newEnds = mergeEnds(p.endings.getOrElse(x,Nil),
                            p.endings.getOrElse(y,Nil))
    val nv = s"${x}_${y}"
    Plot(Map(),Map(nv->newTr),Map(nv->newEnds),Map(nv->newBgs),Set(x),Set(y))

    //IDEA:
    //  - generate the plot with "verbose", to have matching traces (all sub-trajectories will have a trace)
    //  - pair these traces, and "merge" each of them
    //  - merging means, for each (t,v1) from x, and (t,v2) from y, add (v1,v2)

  def mergeTr(trX:Traces,trY:Traces): Traces =
   //println(s"merging...\n- $trX\n- $trY")
   (trX,trY) match
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Nil::r,a) => Nil::mergeTr(r,a)
    case (a, Nil :: r) => Nil::mergeTr(a, r)
    case ((x::xs)::rx, (y::ys)::ry) if x._1==y._1 =>
      //println("case a")
      def mbAgain[A](a:A,as:List[A],bs:List[A]) =
        if as.nonEmpty && bs.isEmpty then  a::as else as
      mergeTr(mbAgain(x,xs,ys)::rx,mbAgain(y,ys,xs)::ry)  match
        case hd::tl => ((x._2,y._2)::hd)::tl
        case Nil => List(List((x._2,y._2)))
    case ((x::xs)::rx, (y::ys)::ry) if (x._1) > (y._1) =>
      //println("case b")
      mergeTr(xs::rx,trY)
    case ((x::xs)::rx, (y::ys)::ry) =>
      //println(s"case c - ${x._1} > ${y._1}")
      mergeTr(trX,ys::ry)


  def mergeBgs(mx:MarkedPoints,my:MarkedPoints): MarkedPoints = (mx,my) match
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (x::xs,y::ys) if x._1==y._1 => (x._2,y._2,if x._3.size>y._3.size then x._3 else y._3) :: mergeBgs(xs,ys)
    case (x::xs,y::ys) if x._1 >y._1 => mergeBgs(xs,my)
    case (x::xs,y::ys)               => mergeBgs(mx,ys)


  def mergeEnds(px:Points,py:Points): Points = (px,py) match
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (x::xs,y::ys) if x._1==y._1 => (x._2,y._2) :: mergeEnds(xs,ys)
    case (x::xs,y::ys) if x._1 >y._1 => mergeEnds(xs,py)
    case (x::xs,y::ys)               => mergeEnds(px,ys)




