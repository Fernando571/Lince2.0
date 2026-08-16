package lince.backend.plot

import lince.backend.plot.Plot.{MarkedPoints, Points, Traces}
import lince.syntax.Lince.PlotInfo

/**
 * Builds JavaScript code to generate Plotly graphs from a given plot
 */
object PlotToJS:

  /** Converts a plot to JavaScript instructions for Plotly */
  def apply(plot: Plot, divName: String, pi: PlotInfo): String =
    colours = (0, Map())
    val vars = plot.traces.keys.toList.sorted
    s"""var colors = Plotly.d3.scale.category10();
       |${setDataJS(plot,divName,pi,vars:List[String])}
       |var layout = {hovermode:'closest',
       |   xaxis: {title: {text: '${plot.xlabels.toList.sorted.mkString("/")}' } },
       |   yaxis: {title: {text: '${ plot.ylabels.toList.sorted.mkString("/")
                                     /*vars.mkString("/")*/}' } },
       |   height: ${pi.height}
       |};
       |Plotly.newPlot('$divName', data, layout, {showSendToCloud: true});
       |""".stripMargin

  /** Adds a plot as an overlay to an existing plot */
  def addPlot(plot: Plot, divName: String, pi: PlotInfo): String =
    val vars = plot.traces.keys.toList.sorted
    s"""${setDataJS(plot, divName, pi, vars)}
       |Plotly.addTraces('$divName', data);
       |""".stripMargin

  private def setDataJS(plot: Plot, divName: String, pi: PlotInfo, vars: List[String]): String = {
    val lbl = if pi.runs > 1 then s"[${pi.runs}] " else ""
    s"""${traceToJS(plot.traces, lbl)}
       |
       |${markEndings(plot.endings,lbl)}
       |
       |${markBeginning(plot.beginnings,lbl)}
       |
       |var data = [${
      (//plot.traces.keys.map("t_"+_).toList ++
        vars.map("t_" + _) ++
          plot.endings.keys.map("end_" + _).toList ++
          plot.beginnings.keys.map("beg_" + _).toList
        ).mkString(",")
    }];""".stripMargin
  }

  /** Converts the traces (lines) to JS for Plotly. */
  def traceToJS(tr: Map[String, Traces],lbl: String): String =
    var js = ""
    for (variable, traces) <- tr do
      val tr = traces
        .filter(_.nonEmpty)
        .map(tr => tr.head.copy(_2 = "null") :: tr).flatten.drop(1)
      val (xs, ys) = tr.unzip
      js +=
        s"""var t_$variable = {
           |   x: ${xs.mkString("[", ",", "]")},
           |   y: ${ys.mkString("[", ",", "]")},
           |   mode: 'lines',
           |   line: {color: colors(${colour(variable)})},
           |   legendgroup: 'g_${variable}_${lbl}',
           |   name: '$lbl${variable}'
           |};
           |""".stripMargin
    js

  /** Converts the beginning markers to JS for Plotly. */
  def markBeginning(ps: Map[String, MarkedPoints], lbl: String): String =
    var js = ""
    for (variable, points) <- ps do {
      val (xs, ys, acts) = points.unzip3
      js +=
        s"""var beg_${variable} = {
           |    x: ${xs.map(x => s"$x,$x").mkString("[", ",", "]")},
           |    y: ${ys.mkString("[", ",null,", "]")},
           |    text: [${acts.map(x => s"'${x.reverse.mkString("<br>")}'").mkString(",null,")}],
           |    mode: 'markers',
           |    marker: {color: colors(${colour(variable)}),
           |      size: 10,
           |      line: {
           |        color: colors(${colour(variable)}),
           |        width: 2
           |    }},
           |    type: 'scatter',
           |    legendgroup: 'g_${variable}_$lbl',
           |    name: 'beginning of $lbl${variable}',
           |    showlegend: false,
           |};
           |""".stripMargin
    }
    js

  /** Converts the ending markers to JS for Plotly. */
  def markEndings(ps: Map[String, Points], lbl: String): String =
    var js = ""
    for (variable, points) <- ps do {
      val (xs, ys) = points.unzip
      js +=
        s"""var end_${variable} = {
           |    x: ${xs.map(x => s"$x,$x").mkString("[", ",", "]")},
           |    y: ${ys.mkString("[", ",null,", "]")},
           |    text: [],
           |    mode: 'markers',
           |    marker: {color: 'rgb(255, 255, 255)',
           |      size: 10,
           |      line: {
           |        color: colors(${colour(variable)}),
           |        width: 2
           |    }},
           |    type: 'scatter',
           |    legendgroup: 'g_${variable}_$lbl',
           |    name: 'ending of $lbl${variable}',
           |    showlegend: false,
           |};
           |""".stripMargin
    }
    js

  ////////////////
  /** Auxiliar function to use consistent colours for all elements to the same variable. */
  private var colours: (Int, Map[String, Int]) = (0, Map())

  private def colour(x: String) = colours._2.get(x) match
    case Some(i) => i
    case None =>
      val oldCol = colours._1
      colours = (oldCol + 1, colours._2 + (x -> oldCol))
      oldCol


