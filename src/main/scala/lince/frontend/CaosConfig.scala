package lince.frontend

import caos.frontend.Configurator.*
import caos.frontend.{Configurator, Documentation}
import caos.view.*
import lince.backend.*
import lince.backend.plot.*
import lince.syntax.Lince.{Action, PlotInfo, Program, Simulation}
import lince.syntax.{Lince, Show}
import SmallStep.{St,initial}
import caos.frontend.widgets.WidgetInfo.Custom

import scala.util.Random

/** Object used to configure which analysis appear in the browser */
object CaosConfig extends Configurator[Simulation]:
  val name = "Lince 2.0: Simulator of Cyber-Physical Systems as Hybrid Programs"
  override val languageName: String = "Input program"

  /** Parser, converting a string into a System in Lince 2.0 */
  val parser =
    lince.syntax.Parser.parseSimulation

  /** Examples of programs that the user can choose from. The first is the default one. */
  import lince.frontend.examples.Basic
  val examples =
      Basic.toy ++
      Basic.scenarios ++
      Basic.prob ++
      Basic.configs
                 

  // val examples = List(
  //   // "sin-bug"
  //   //   -> "theta := 0.01;\na := 0;\ntheta' = a, a' = sin(theta) for 40;\n--\nuntil 40\nsamples 150"
  //   //   -> "Buggy experiment with ODEs with sin.",
  //   "CC" -> "// Cruise control\nx:=0; v:=2;\nwhile true do {\n  if v<=10\n  then x'=v,v'=5  for 1;\n  else x'=v,v'=-2 for 1;\n}\n--\nuntil 5"
  //     -> "Simple cruise control system, used to illustrate basic functionality of Lince.",
  //   "CC (broken)" -> "// Cruise control\nx:=0; v:=2;\nwhile true do {\n  if v<=10\n  then x'=v,v'=5  for 1;\n  else x'=v,v'=-2 for 1;\n  if x>=10 then x:=10;\n}\n--\nuntil 5\nsamples 10"
  //     -> "Variation of the cruise control example, breaking the continuity of the trajectories.",
  //   "Simple composition"
  //     -> "p:=0; v:=0;\np'=v,v'= 2  for 5;\np'=v,v'=-2  for 5;"
  //     -> "Composing two trajectories: the 1st accelerates (2) and the 2nd brakes (-2).",
  //   "x:=2" -> "x := 2;",
  //   "skip" -> "skip;",
  //   "Bouncing ball"
  //     -> "// Bouncing ball example\nv:=5; p:=10; c:=0;\nwhile (c<4) do {\n  v'= -9.8, p'=v until_0.05 p<0 && v<0;\n  v:= -0.5*v; c:=c+1;\n}\n--\niterations 1000 "
  //     -> "<strong>Bouncing Ball</strong><p>Event-Driven (ED) example, using steps of 0.01. A ball position and velocity as it bounces in the floor. It includes an experimental feature: using a condition (p<0 /\\ v<0) to end a trajectory using a naive search algorithm.</p>",
  //   "Firefiles"
  //     -> "f1 := 1; f2 := 4;\nwhile true {\n  f1'=1, f2'=1 until_0.1\n       f1>10 || f2>10;\n  if f1>=10 && f2<10\n    then { f1:=0; f2:=f2+2; }\n    else if f2>=10 && f1<10\n         then { f2:=0;f1 :=f1 +2; }\n         else { f1:=0; f2 :=0; }\n}\n--\nuntil 30\niterations 1000"
  //     -> "<strong>Fireflies 2x</strong>\n\nEvent-Driven (ED) example. Every firefly has an internal clock that helps it to know when to flash: when the clock reaches a threshold the firefly flashes and the clock’s value is reset to zero. If other fireflies are nearby then they try to synchronise their flashes in a decentralised way.This version synchronises 2 fireflies.",
  //   "PPDP - Ex.2.1"
  //     -> "x := 0 ; c := 0 ;\nwhile c <= 5 do {\n  bernoulli (1/2)\n    x++; x--;\n  c++;\n} \nx := x/sqrt(5);"
  //     -> "Example 2.1 - An execution sample of a random walk with 50% chances of increasing or decreasing a variable x.",
  //   "PPDP - Ex.2.2"
  //     -> "x := 0;\nwhile true {\n  bernoulli (1/2)\n    x++; x--;\n  wait unif(0,1);\n}"
  //     -> "Example 2.2 - An execution sample of a continuous-time random walk in which the waiting time is given by sampling from the uniform distribution on [0,1].",
  //   "PPDP - Ex.2.3"
  //     -> "d:=0;\np := 10 ; v := 0 ;\nwhile true {\n  d := unif (2,4) ;\n  p'=v,v'= -9.8 for d ;\n  v := -v;\n}"
  //     -> "Example 2.3 - An execution sample of the ball's position (p) and velocity (v) during the first 5 time units.",
  //   "PPDP - Ex.2.4"
  //     -> "lambda:=2; d:=0;\n// the \"seed\" variable fixes\n// the pseudo-random generator\np:=0; v:=0; a:=0;\nwhile true {\n  d:=expn(lambda);\n  bernoulli (1/2)\n    a--; a++;\n  p'= v, v'=a for d;\n}\n---\nruns 20\nuntil 15\nvars p"
  //     -> "Example 2.4 - Multiple execution samples of the particle’s position overlayed, in order to depict how the position’s probability mass spreads over space w.r.t time.",
  //   "PPDP - Ex.2.5"
  //     -> "x:=expn(2); y:=expn(2);\np:=0; v:=0;\np'=v, v'=1  for sqrt (3) + x;\np'=v, v'=-1 for sqrt (3) + y;"
  //     -> "Example 2.5 - Execution sample of a particle's position (p) and velocity (v).",
  //   "PPDP - Ex.2.6a"
  //     -> "// Adaptive Cruise Control (ACC)\np:=0; v:=0;  // follower  \npl:=50; vl:=10; // leader\na:=0;\nwhile true {\n  // decide to speed up (acc=2) or brake (acc=-2)\n  if (v-8)^2 + 4*(p-pl+v-9) < 0\n  then p'=v, v'= 2, pl'=vl, vl'=a for 1;\n  else p'=v, v'=-2, pl'=vl, vl'=a for 1;\n}\n----\nuntil 20\nvars p.*"
  //     -> "Example 2.6a - Adaptive cruise control with a leader with constant acceleration (no uncertainty).",
  //   "PPDP - Ex.2.6b"
  //     -> "// Adaptive Cruise Control (ACC)\np:=0; v:=0;  // follower  \npl:=50; vl:=10; // leader\na:=0;\nwhile true {\n  a := unif(-1,1) ;\n  // decide to speed up (acc=2) or brake (acc=-2), assuming a==-1\n  if (v - vl + 3)^2 + 4*(p - pl + v - vl + 3/2) < 0\n  // uncomment the \"if\" below to try version 2.6a (incorrect)\n  // if (v-8)^2 + 4*(p-pl+v-9) < 0\n  then p'=v, v'= 2, pl'=vl, vl'=a for 1;\n  else p'=v, v'=-2, pl'=vl, vl'=a for 1;\n}\n----\nuntil 20\nseed 10\nvars p.*"
  //     -> "Example 2.6b Adaptive cruise control with a leader with an uncertain acceleration (bounded by fixed values).",
  //   "PPDP - Ex.2.6c"
  //     -> "// Adaptive Cruise Control (ACC)\n// (unexpected delay)\np := 0; v := 0; // follower  \npl:=50; vl:=10; // leader\nlambda:=2;\nwhile true {\n  x := expn(lambda) ; x++ ;\n  // decide to speed up (acc=2) or brake (acc=-2)\n  if (v-8)^2 + 4*(p-pl+v-9) < 0\n  then p'=v, v'= 2, pl'=vl, vl'=0 for x;\n  else p'=v, v'=-2, pl'=vl, vl'=0 for x;\n}\n----\nuntil 20\nvars p.*"
  //     -> "Example 2.6c: Adaptive cruise control with a leader with an uncertain delay during rounds (unbounded, given by a exponential distribution).",
  //   "normal-dist"
  //     -> "while true {\n\t// normal distribution with mean 5 and std. deviation 2\n  d:=normal(5,2);\n  // try also a negative dist. mean 2:\n  //d:=expn(1/2);\n\twait 1;\n}\n---\nruns 50\nuntil 1\nvars d"
  //     -> "Sampling from a normal distribution (or other).",
  //   "Single tank (poll-variation)"
  //     -> "// Define initial values of the water tank\nlevel := 5;\ndrain := -1/2;\n\nwhile true do {\n  // keep level between 3..10\n  if      level<=3  then drain:= 1/2;\n  else if level>=10 then drain:=-1/2;\n  else    skip;\n\n  level'= drain, drain'=0\n    for 0.1;\n}\n---\nuntil 100\niterations 1000",
  //   "Water tank"
  //     -> "a1:=1; //Area of tank 1\na2:=1; // Area of tank 2\nr1:=1; //Resistance applied to the water flow at the water exit tap of tank 1.\nr2:=10; //Resistance applied to the water flow at the water exit tap of tank 2.\nh1_p:=10; // initial water level of tank 1 in the aligned  configuration\nh2_p:=0; // initial water level of tank 2 in the aligned  configuration.\nh1_v:=10; //initial water level of tank 1 in vertical configuration.\nh2_v:=0; //initial water level of tank 2 in vertical  configuration.\n\n// Open tap of the tank 1 and close the tap of the tank 2\nqe1:=1;\nqe2:=0;\n\n// Differential equations simulating the variation of the water level in the two tanks, in each configuration, after 50 seconds.\nh1_p'=-pow(a1*r1,-1)*h1_p+pow(a1*r1,-1)*h2_p+pow(a1,-1)*qe1,\nh2_p'=pow(a2*r1,-1)*h1_p-pow(a2*r1,-1)*h2_p+pow(a2,-1)*qe2-pow(a2*r2,-1)*h2_p,\nh1_v'=-pow(a1*r1,-1)*h1_v+pow(a1,-1)*qe1,\nh2_v'=pow(a2*r1,-1)*h1_v-pow(r2*a2,-1)*h2_v + pow(a2,-1)*qe2 for 40;\n\n// Open tap of the tank 2 and close the tap of the tank 1\nqe1:=0;\nqe2:=1;\n\n// Differential equations simulating the variation of the water level in the two tanks, in each configuration, after 50 seconds.\nh1_p'=-pow(a1*r1,-1)*h1_p+pow(a1*r1,-1)*h2_p+pow(a1,-1)*qe1,\nh2_p'=pow(a2*r1,-1)*h1_p-pow(a2*r1,-1)*h2_p+pow(a2,-1)*qe2-pow(a2*r2,-1)*h2_p,\nh1_v'=-pow(a1*r1,-1)*h1_v+pow(a1,-1)*qe1,\nh2_v'=pow(a2*r1,-1)*h1_v-pow(r2*a2,-1)*h2_v + pow(a2,-1)*qe2 for 40;\n\n//Open both\nqe1:=1;\nqe2:=1;\n\n// Differential equations simulating the variation of the water level in the two tanks, in each configuration, after 50 seconds.\nh1_p'=-pow(a1*r1,-1)*h1_p+pow(a1*r1,-1)*h2_p+pow(a1,-1)*qe1,\nh2_p'=pow(a2*r1,-1)*h1_p-pow(a2*r1,-1)*h2_p+pow(a2,-1)*qe2-pow(a2*r2,-1)*h2_p,\nh1_v'=-pow(a1*r1,-1)*h1_v+pow(a1,-1)*qe1,\nh2_v'=pow(a2*r1,-1)*h1_v-pow(r2*a2,-1)*h2_v + pow(a2,-1)*qe2 for 40;\n\n//Close both\nqe1:=0;\nqe2:=0;\n\n// Differential equations simulating the variation of the water level in the two tanks, in each configuration, after 50 seconds.\nh1_p'=-pow(a1*r1,-1)*h1_p+pow(a1*r1,-1)*h2_p+pow(a1,-1)*qe1,\nh2_p'=pow(a2*r1,-1)*h1_p-pow(a2*r1,-1)*h2_p+pow(a2,-1)*qe2-pow(a2*r2,-1)*h2_p,\nh1_v'=-pow(a1*r1,-1)*h1_v+pow(a1,-1)*qe1,\nh2_v'=pow(a2*r1,-1)*h1_v-pow(r2*a2,-1)*h2_v + pow(a2,-1)*qe2 for 40;\n---\nuntil 100\nsamples 60\nvars h.*",
  //   "Two heated tanks"
  //     -> "a1 := 0.01; b1 := 0.005;\na2 := 0.01; b2 := 0.005;\n\nk11_l1 := 0.30760137; k12_l1 := 0.07706312;\nk21_l2 := 0.07706312; k22_l2 := 0.30760137;\n\nw1 := 80; w2 := 100; // set-point\nd := 0.5;   // tolerance\n\nx1:=0; x2:=50; // initial state\ni := 0;  // counter\nn := 10;  // max jumps\np := 0.5; // max evolution period (affects precision)\n\nwhile i<n {\n   // L1\n   x1'=-a1*x1+b1*x2-(k11_l1*x1+k12_l1*x2) + (k11_l1*w1+k12_l1*w2),\n   x2'=b2*x1-a2*x2\n   until_p x1>=w1-d;\n   // L0\n   x1'=-a1*x1+b1*x2,\n   x2'=b2*x1-a2*x2\n   until_p x1<=80+d && x2<=80+d;\n   // L2\n   x1'=-a1*x1+b1*x2,\n   x2'=b2*x1-a2*x2-(k21_l2*x1+k22_l2*x2) + (k21_l2*w1+k22_l2*w2)\n   until_p x2>=w2-d;\n   // L0\n   x1'=-a1*x1+b1*x2,\n   x2'=b2*x1-a2*x2\n   until_p x1<=80+d && x2<=80+d;\n   i++;\n}\n---\nvars x1, x2, i\nuntil 1000\niterations 500"
  //     -> "Variation of a system of two heated tanks from the literature.",
  //   "Simple portrait"
  //     -> "// Solution not naively computed (precise solution involves sin/cos)\n// Use the online version to use the precise solution.\np:=1;v:=1;\np'=v, v'=-p for 4;\n--\nportrait p,v\nuntil 4"
  //     -> "Simple portait of two waves.",
  //   "Pursuit portrait"
  //     -> "// Initial position and velocity of the missile\nx:=300; vx:=20; y:=300; vy:=0;\n// Initial position and velocity of the target\nxl:=500; vxl:=15; yl:=500; vyl:=0;\n\n// Angular velocity of the missile and target (leader)\naw:=(1/20)*2*pi();\nawl:=(1/40)*2*pi();\n\n// Counter\ncont:=0;\n// Decision time\nsampling_time:=1;\n// Minimum collision distance\ndist_min_col:=1; \n// variable that stores the alpha angle\nalpha:=0;\n//Variable that stores the vectorial product to decide which way to turn\nvect_P:=0;\n// Variables that stores the angular velocity decision to the missile and the target\nw:=0; wl:=0;\n//Variables that stores the relative positions and velocities\ndx:=0; dy:=0; vrelx:=0; vrely:=0;\n\n// Run the following programme whilst the distance between the missile and the target is greater than \n//the collision distance\nwhile (sqrt((x-xl)^2+(y-yl)^2)>dist_min_col) do {\n    //Conditional structures to establish the target path\n    if (cont<=10)\n    then wl:=0;\n    else {\n          if (cont<=20)\n          then wl:=-awl;\n          else {\n              if (cont<=30)\n              then wl:=awl;\n              else wl:=0;\n              }\n        }\n    // The counter is incremented\n    cont:=cont+1;\n    //Update distances and relative velocities\n    dx:=xl-x;\n    dy:=yl-y;\n    vrelx:=vxl-vx;\n    vrely:=vyl-vy;\n    // Determine the value of the angle alpha\n    alpha:=arccos((vrelx*dx + vrely*dy)/(sqrt(vrelx^2 + vrely^2)*sqrt(dx^2 + dy^2))); \n    // Conditional structures to determine whether the missile needs to move forward or make a curve\n    if (alpha>=179.5*pi()/180 && alpha<=180.5*pi()/180)\n    then // If the theta is  between 179.5 and 180.5 degrees, the missile follows a straight line at a constant velocity \n         w:=0;\n    else {\n        // Determine the value of the vetorial product between the relative velocity vector and the relative position vector\n        vect_P:=vrelx*dy-vrely*dx;\n        // If the theta is not between 179.5 and 180.5 degrees, the missile needs to curve to the left or right\n        // To decide which way to turn, simply check the sign of the vectorial product. \n        if (vect_P>=0)\n        then // If the vectorial product is positive or zero,  it curves to the right\n             w:=aw;\n          else // If the vectorial product is negative,  it curves to the left\n               w:=-aw;\n        }\n    // Differential equations\n    x'=vx,y'=vy,vx'=w*vy,vy'=-w*vx,\n    xl'=vxl,yl'=vyl,vxl'=wl*vyl,vyl'=-wl*vxl for sampling_time;\n}\n--\nuntil 50\nportrait x,y; xl,yl\nsamples 80"
  //     -> "Missile pursuing a target (2D)\n\nMissile trajectory that follows a given target.",
  //   "FMAS CC"
  //     -> "// Adaptive Cruise Control (ACC) \nfwd:=3; bwd:=-3; // constants\npl :=50; vl := 0; al:=1; // [-3..3]; //leader \npf := 0 ; vf := 0; af := fwd;  //follower\ndiscr := 0; bt:=0; at:=0; ct:=0;\nst := 2; //sample time\nwhile true do {\n bt:= (al-fwd)*st+vl-vf; \n at:= (al-bwd)/2; \n ct:= (((al-fwd)/2)*st^2+(vl-vf)*st+pl-pf);\n discr:= bt^2 - 4*at*ct;\n if ct<=0 || \n   (at==0 && bt!=0 && -ct/bt > 0) || \n    (discr >= 0 && at!=0 &&\n     ((-bt - sqrt(discr))/(2*at) > 0  ||\n      (-bt + sqrt(discr))/(2*at) > 0 ))  \n then af :=bwd;  //brake \n else af :=fwd; //accelerate   \n//update states\n  pf'=vf, vf'=af,\n  pl'=vl, vl'=al for st;}\n---\nvars pl,pf\nuntil 15\n"
  //     -> "Adaptive Cruise Control example, used in FMAS'25",
  //   // "Sin(x)"
  //   //   -> "x:=0;\n// Trying to simulate sin(x) using ODEs\nwhile x<=10 do {\n  y:=sin(x);\n  x'=1, y'=cos(x) for 0.1;\n}\n---\nuntil 10\nvars y"
  //   //   -> "Trying to simulate sin(x) using ODEs.", //" Example suggested by LLM (probably by Pedro R. D'Argenio).",
  //   "Sin(x)"
  //     -> "x:=0; y:=0;\n// Simulating sin(x) using ODEs\nwhile true\n  x'=1, y'=cos(x) for 5;\n---\nuntil 20\nrk-samples 2 // change to 1 to drop precision\nsamples 100  // change to 20 to view less points\nverbose\nvars y\n"
  //     -> "Simulating sin(x) using ODEs. Change the <code>rk-samples</code> and the (plot) <code>samples</code> to experiment with configurations of the simulation.",
  //   "Precision test"
  //     -> "theta := 0.1;\na := 0;\ntheta' = a,\n    a' = sin(theta) for 120;\n\n---\nuntil 30\nsamples 80\nrk-samples 18 // increase (100) to fix example\n"
  //     -> "Example with a solution that requires enough precision to be computed correctly. Increase the number of <code>rk-samples</code> (used in the Runge Kutta), e.g., to <code>100</code>, to improve precision.",
  // )

  /** Description of the widgets that appear in the dashboard. */
  val widgets = List(
    "View parsed" -> view(_.toString,Text).moveTo(1),
    "View pretty" -> view[Simulation](s=>Show(s._1),Code("clike")).moveTo(1),
    "Plots"
      -> Custom[Simulation](divName = "sim-plotlys", reload = sim => {
          val plots = Plot.allPlots(initial(sim), sim.pi)
          val js = PlotToJS(plots.head._1, "sim-plotlys", plots.head._2) + "\n" +
                   plots.tail.map(p=>PlotToJS.addPlot(p._1, "sim-plotlys", p._2)).mkString("\n")
          scala.scalajs.js.eval(js)
        }, buttons = Nil).expand,
    "Run small-steps" -> steps[Simulation,Action,St]
      (initial, SmallStep, Show.simpleStML, _.toString, Text),
    "Run all steps" -> lts[Simulation,Action,St]
      (initial, SmallStep, Show.simpleStML, _.toString),
    // "Run all steps (inf)" -> lts[Simulation,Action,St]
    //   (_.state, StillSmallStep, Show.simpleStML, _.toString),
    "Final state" -> view[Simulation](sim => Show.simpleStML(BigSteps.bigStep(initial(sim),Nil)(using sim.pi.rkSamples)._2),Text),
    "Plot debug"
      -> view[Simulation](sim=> {
            val ps = Plot.justPlot(initial(sim), sim._2)
            if sim.pi.portrait.nonEmpty then
              Plot.justPlot(initial(sim), sim._2.copy(portrait=Nil)).head.show + "\n---\n" + ps.map(_.show).mkString("\n\n")
            else
              ps.map(_.show).mkString("\n\n")
          },
          Text),
    "Plots JS"
      -> view(sim => {
            val plots = Plot.allPlots(initial(sim), sim.pi)
            PlotToJS(plots.head._1, "sim-plotlys", plots.head._2) + "\n" +
              plots.tail.map(p=>PlotToJS.addPlot(p._1, "sim-plotlys", p._2)).mkString("\n")
          }, Text),
    "Plot2trace debug"
      -> view(sim => {
            val sim2 = if sim.pi.portrait.nonEmpty then
              sim.copy(pi=sim.pi.copy(portrait=Nil,
                         showVar=sim.pi.portrait.flatMap(x=>List(x._1,x._2)).contains))
              else sim
            val ps = Plot.justPlot(initial(sim2), sim2._2)
            PlotToTrace(ps.head).map(kv => roundf(kv._1).toString + ": " +
                kv._2.map(x => s"${x._1} -> ${roundf(x._2)}").mkString(", ")).mkString("\n")
          },
          Text),
    // "Periodic samples"
    //   -> view(sim => {
    //         val ps = Plot(sim.state, sim._2)
    //         val samples = sim.pi.monSampleFreq
    //         ps.map(p => p.sample(samples).map(kv => roundf(kv._1).toString + ": " +
    //             kv._2.map(x => s"${x._1} -> ${roundf(x._2)}").mkString(", ")).mkString("\n\n")).mkString("\n\n")
    //       },
    //       Text),

//    "Plot"
//      -> Custom[Simulation](divName = "sim-plotly", reload = sim => {
//          val js = PlotToJS(Plot(sim.state, sim.pi), "sim-plotly", sim.pi)
//          scala.scalajs.js.eval(js)
//        }, buttons = Nil).expand,
//    "Plot JS"
//      -> view[Simulation](sim=>
//          PlotToJS(Plot(sim.state, sim._2), "sim-plotly", sim.pi),
//          Text),
  )
  def round(x:Double):Double =
    BigDecimal(x).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
  def roundf(x:Double) =
    BigDecimal(x).setScale(3, BigDecimal.RoundingMode.HALF_UP)

  
  override val toggles: List[Toggle] = List(
    "Basic" -> Basic.toy.map(_.name).toSet -> false,
    "Scenarios" -> Basic.scenarios.map(_.name).toSet,
    "Probab." -> Basic.prob.map(_.name).toSet,
    "Config." -> Basic.configs.map(_.name).toSet -> false,
    "Debug" -> Set("Plot debug", "Plot2trace debug", "Plots JS", "View parsed", "View pretty") -> false
  )

  //// Documentation below

  override val footer: String =
    """Simple animator of Lince 2.0, meant for cyber physical systems, describing programs with discrete and continuous evolution.
      | Source code available online:
      | <a target="_blank" href="https://github.com/arcalab/lince-2.0">
      | https://github.com/arcalab/lince-2.0</a>. Used to illustrate a stochastic extension described in a <a href="https://jose.proenca.org/publication/ppdp-stochastic-lince-2025/">PPDP'25 publication and presentation</a>.
      | This updated version still includes less features than its predecessor at <a href="http://arcatools.org/lince">http://arcatools.org/lince</a>.""".stripMargin

  override val documentation: Documentation = List(
    languageName -> "More information on the syntax of Lince 2.0" ->
        // |  b ::= e <= e  |  b && b  |  b || b  |  true  |  false
      """<p>A program <code>p</code> in Lince 2.0 is given by the following grammar:
        |<pre>
        |  p ::= a  |  skip  |  p p  |  if e [then] p else p  |  while e p  |  { p } 
        |  a ::= x1'=e, ...,xn'=e for e;  |  x:=e;  |  (@keep)? def x := s;
        |  e ::= x  |  r  |  true  |  false  |  f(e,...,e)
        |  s := e  |  [r1,r2,...]
        |</pre></p>
        |<p> Where <code>r</code> is a real number, <code>(@keep)?</code> means that <code>@keep</code> is optional, and <code>def x:= s</code> defines a (possibly infinite or empty) stream of numbers or a constant (non-recursive) expression.</p>
        |<p> Known functions for <code>f</code> include <code>&&</code>, <code>||</code>, <code>==</code>, <code>!=</code>, <code>&lt;</code>, <code>*</code>, <code>/</code>, <code>+</code>, <code>-</code>, <code>^</code>, <code>pow</code>, <code>sqrt</code>, <code>exp</code>, <code>sin</code>, <code>cos</code>, <code>tan</code>, <code>cosh</code>, <code>sinh</code>, <code>tanh</code>, <code>pi</code>, <code>unif</code>, <code>expn</code>, <code>normal</code>, <code>powerlaw</code>.</p>
        |<p> You can customize your plot by appending to the end of your program, e.g.,
        |<pre>
        |---
        |until 5 // maximum time (default 10)
        |from 0 // starting time (default 0)
        |iterations 10 // maximum times the while loops are unfolded (default 500)
        |samples 40 // minumum number of points to be sampled when drawing the plot (default 20)
        |rk-samples 20 // number of samples for the Runge-Kutta method (default 100)
        |seed 0 // seed for the random generator  (every time a random one by default)
        |vars x.*, y // list of regular expressions to select variables to be displayed (default all)
        |height 450 // sets the height in px of the graph (default 450)
        |runs 5 // number of plots to draw (default 1, useful for random plots)
        |portrait x,y; z,w // creates a portrait plot, relating x with y and z with w (deactivated by default)
        |verbose // shows a marker at every discrete step (does not show by default)
        |</pre>
        |</p>
        |<h3>Note on stochastic functions</h3>
        |<p> The functions below are stochastic, and will yield a different value every time they are called. For reproducibility, you can set the "seed" value to fix the internal pseudo-random generator.
          <ul>
            <li><code>unif(a,b)</code> - random value from a uniform distribution between <code>a</code> and <code>b</code>;</li>
            <li><code>unif()</code> - equivalent to <code>unif(0,1)</code></li>
            <li><code>expn(lambda)</code> - random variable from a negative exponential distribution, using an average frequence of <code>lambda</code> (equivalent to <code>-ln(unif()) / lambda</code>);</li>
            <li><code>normal(m,s)</code> - random variable from a normal distribution with mean <code>m</code> and standard deviation <code>s, using the Box-Muller method.</code></li>
            <li><code>powerlaw(alpha,xmin)</code> - random variable from a power law distribution, using a normalisation value <code>alpha > 1</code> and a lower bound <code>xmin</code> for the returned values (equivalent to <code>xmin * unif()^(−1/(alpha−1))</code>).</li>
          </ul>
        |</p>
        |""".stripMargin,
    "Run small-steps" -> "Information on the semantics rules used by Lince" ->
      "<p>The operational rules, borrowed from a <a href=\"https://jose.proenca.org/publication/ppdp-stochastic-lince-2025/\">PPDP'25 publication</a>, can be found below.</p><img src=\"img/lince-semantics.svg\" style: width=50%;s/>"
//      "<object class=\"pdf\" data=\"img/lince-semantics.pdf\" width=\"800\" height=\"500\"></object>",
    )
