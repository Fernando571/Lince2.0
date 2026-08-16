package lince.frontend.examples

import caos.frontend.Configurator.Example

object Basic:
  val toy: List[Example]  = List(
    "Accelerate" -> "p:=0; v:=0;\np'=v,v'= 2  for 5;"
        -> "A simple system that accelerates for 5 seconds.",
    "Wave" -> "x:=1; y:=0;\nx'=y,y'=-x for 10;"
        -> "A simple system that produces a wave trajectory.",
    "Pendulum" -> "theta := 0.1;\na := 0;\ntheta' = a,\n    a' = -sin(theta) for 10;"
        -> "A simple system that simulates the trajectory of a pendulum.",
    "Contiguous composition"
        -> "p:=0; v:=0;\np'=v,v'= 2  for 5;\np'=v,v'=-2  for 5;"
        -> "Composing two trajectories: the 1st accelerates (2) and the 2nd brakes (-2).",
    "Broken composition"
        -> "p:=0; v:=0;\np'=v,v'= 2  for 5;\np:=5;\np'=v,v'=-2  for 5;\np:=0;"
        -> "Variation of the previous example, breaking the continuity of the trajectories.",
    "Naive particle positioning"
        -> "x:=0; v:=0;\nwhile true do {\n  if x<0\n  then x'=v,v'= 1 for 1;\n  else x'=v,v'=-1 for 1;\n}\n--\nuntil 50"
        -> "A simple system that tries to position a particle at a target position (0). The particle accelerates or brakes depending on its current position.",
  )

  val scenarios: List[Example] = List(
    "Cruise control" -> "// Cruise control\nx:=0; v:=2;\nwhile true do {\n  if v<=10\n  then x'=v,v'=5  for 1;\n  else x'=v,v'=-2 for 1;\n}\n--\nuntil 5"
        -> "Simple cruise control system, used to illustrate basic functionality of Lince.",
    // "CC (broken)" -> "// Cruise control\nx:=0; v:=2;\nwhile true do {\n  if v<=10\n  then x'=v,v'=5  for 1;\n  else x'=v,v'=-2 for 1;\n  if x>=10 then x:=10;\n}\n--\nuntil 5\nsamples 10"
    //     -> "Variation of the cruise control example, breaking the continuity of the trajectories.",
    // "x:=2" -> "x := 2;",
    // "skip" -> "skip;",
    "Adaptive CC (no accel)"
      -> "// Adaptive Cruise Control (ACC)\np:=0; v:=0;  // follower  \npl:=50; vl:=10; // leader\nal:=0;\nwhile true {\n  // decide to speed up (acc=2) or brake (acc=-2)\n  if (v-8)^2 + 4*(p-pl+v-9) < 0\n  then p'=v, v'= 2, pl'=vl, vl'=al for 1;\n  else p'=v, v'=-2, pl'=vl, vl'=al for 1;\n}\n----\nuntil 20\nvars p.*"
      -> "Simple adaptive Cruise Control example, whereas a follower tries to keep a safe distance from a leader. The follower can either accelerate or brake, and the leader keeps a constant speed.",
    "Adaptive CC (FMAS'25)"
      -> "// Adaptive Cruise Control (ACC) \nfwd:=3; bwd:=-3; // constants\npl :=50; vl := 0; al:=1; // [-3..3]; //leader \npf := 0 ; vf := 0; af := fwd;  //follower\ndiscr := 0; bt:=0; at:=0; ct:=0;\nst := 2; //sample time\nwhile true do {\n bt:= (al-fwd)*st+vl-vf; \n at:= (al-bwd)/2; \n ct:= (((al-fwd)/2)*st^2+(vl-vf)*st+pl-pf);\n discr:= bt^2 - 4*at*ct;\n if ct<=0 || \n   (at==0 && bt!=0 && -ct/bt > 0) || \n    (discr >= 0 && at!=0 &&\n     ((-bt - sqrt(discr))/(2*at) > 0  ||\n      (-bt + sqrt(discr))/(2*at) > 0 ))  \n then af :=bwd;  //brake \n else af :=fwd; //accelerate   \n//update states\n  pf'=vf, vf'=af,\n  pl'=vl, vl'=al for st;}\n---\nvars pl,pf\nuntil 15\n"
      -> "Adaptive Cruise Control example, used in FMAS'25, where the leader can have a fixed constant acceleration.",
    // "Ball" -> "x:=0; v:=0;\np'=v,v'=-9.8 for 5;"
    //     -> "A simple system that simulates the trajectory of a ball under gravity.",
    "Bouncing ball"
      -> "// Bouncing ball example\nv:=5; p:=10; c:=0;\nwhile (c<4) do {\n  v'= -9.8, p'=v until_0.05 p<0 && v<0;\n  v:= -0.5*v; c:=c+1;\n}"
      -> "<strong>Bouncing Ball</strong><p>Event-Driven (ED) example, using steps of 0.01. A ball position and velocity as it bounces in the floor. It includes an experimental feature: using a condition (p<0 /\\ v<0) to end a trajectory using a naive search algorithm.</p>",
    "Fireflies 2x"
      -> "f1 := 1; f2 := 4;\nwhile true {\n  f1'=1, f2'=1 until_0.1\n       f1>10 || f2>10;\n  if f1>=10 && f2<10\n    then { f1:=0; f2:=f2+2; }\n    else if f2>=10 && f1<10\n         then { f2:=0;f1 :=f1 +2; }\n         else { f1:=0; f2 :=0; }\n}\n--\nuntil 30\niterations 1000"
      -> "<strong>Fireflies 2x</strong>\n\nEvent-Driven (ED) example. Every firefly has an internal clock that helps it to know when to flash: when the clock reaches a threshold the firefly flashes and the clock’s value is reset to zero. If other fireflies are nearby then they try to synchronise their flashes in a decentralised way. This version synchronises 2 fireflies.",
    "Fireflies 3x"
      -> "f1 := 1; f2 := 4; f3 := 7;\nwhile true {\n  f1'=1, f2'=1, f3'=1 until_0.1\n       f1>10 || f2>10 || f3>10;\n  if f1>=10 && f2<10 && f3<10\n    then { f1:=0; f2:=f2+2; f3:=f3+2; }\n    else if f2>=10 && f1<10 && f3<10\n         then { f2:=0;f1 :=f1 +2; f3:=f3+2; }\n         else if f3>=10 && f1<10 && f2<10\n              then {f3:=0;f1 :=f1 +2;f2:=f2+2;}\n              else {f1:=0;f2:=0;f3:=0;}\n}\n--\nuntil 30\niterations 1000"
      -> "<strong>Fireflies 3x</strong>\n\nEvent-Driven (ED) example. Variation of the previous example, synchronising 3 fireflies.",
    "Lotka-Volterra"
      -> "x:=10; y:=5;\nx' = 2*x - x*y,\ny' = -y + x*y for 15;\n---\nuntil 15\nsamples 200"
      -> "Lotka-Volterra predator-prey model. The prey (x) grows exponentially when there are no predators (y), but its growth is limited by the presence of predators. The predators (y) die out when there are no prey (x), but they can grow by consuming prey.",
    "SIR model"
      -> "s:=0.9; i:=0.1; r:=0;\ns'=-s*i, i'=s*i-0.5*i, r'=0.5*i for 15;\n---\nuntil 15\nsamples 200"
      -> "SIR epidemiological model. The susceptible population (s) decreases as it gets infected by the infected population (i), which increases as it infects more people but also decreases as it recovers. The recovered population (r) increases as the infected population recovers.",
    "Inverted pendulum"
      -> "theta := 0.1;\na := 0;\ntheta' = a,\n    a' = -sin(theta) + 0.5 for 10;\n---\nuntil 10\nsamples 200"
      -> "Inverted pendulum example. A pendulum that is trying to stay upright by applying a constant torque (0.5). The system is unstable and the pendulum will eventually fall, but it can stay upright for a while depending on the initial conditions.",
    // "RLC circuit"
    //   -> "v := 10; i := 0;\nv' = 0, i' = v - 2*i for 5;\n---\nuntil 5\nsamples 100"
    //   -> "RLC circuit example. The voltage (v) and current (i) in a simple RLC circuit, where the voltage is constant and the current changes according to the voltage and its own value.",
    // "RLC circuit (ED)"
    //   -> "v := 10; i := 0;\nwhile true do {\n  v' = 0, i' = v - 2*i for 5;\n  v := -v;\n}\n---\nuntil 20\nsamples 100"
    //   -> "Variation of the RLC circuit example, where the voltage changes sign every 5 seconds, simulating an alternating current.",
    "RLC circuit"
      -> "volt:=0; d:=0; v:=0;\nc:=0.047; l:=0.047;\nres:=0.5;\n\nwhile true do {\n  if (volt<10) then v:=18;\n               else v:=0;\n  volt'= d,\n  d'= -(d*res/l)\n      -volt/(l*c)+v/(l*c)\n  for 0.01;\n}\n---\nuntil 0.6\nvars volt"
      -> "<p><strong>RLC circuits and harmonic oscillation</strong></p> This simulation models an electric system composed of a resistor, a capacitor, an inductor, and a power source connected in series. The power source strategically switches on and off, as a way to stabilise voltage across the capacitor at a target value (say, 10V ). Such systems are known to yield interesting results that are practically relevant for energy storage voltage control systems, which help to mitigate voltage imbalances that could otherwise damage electronic equipment.  We simulate one RLC circuit with the capacitor voltage <code>volt</code>, with resistance <code>res</code> of 0.5Ω, a capacitance <code>c</code> of 0.047 F, and an inductance <code>l</code> of 0.047H.  The general idea of our program is that the controller will read the voltage across the capacitor (variable <code>volt</code>) every 0.01 seconds, and set the voltage at the source either to 0 (off) or 18V (on) depending on the value read.",
    "Water tank"
      -> "// Define initial values of the water tank\nlevel := 5;\ndrain := -1/2;\n\nwhile true do {\n  // keep level between 3..10\n  if      level<=3  then drain:= 1/2;\n  else if level>=10 then drain:=-1/2;\n  else    skip;\n\n  level'= drain, drain'=0\n    for 0.1;\n}\n---\nuntil 100\niterations 1000",
    "Two heated tanks"
      -> "a1 := 0.01; b1 := 0.005;\na2 := 0.01; b2 := 0.005;\n\nk11_l1 := 0.30760137; k12_l1 := 0.07706312;\nk21_l2 := 0.07706312; k22_l2 := 0.30760137;\n\nw1 := 80; w2 := 100; // set-point\nd := 0.5;   // tolerance\n\nx1:=0; x2:=50; // initial state\ni := 0;  // counter\nn := 10;  // max jumps\np := 0.5; // max evolution period (affects precision)\n\nwhile i<n {\n   // L1\n   x1'=-a1*x1+b1*x2-(k11_l1*x1+k12_l1*x2) + (k11_l1*w1+k12_l1*w2),\n   x2'=b2*x1-a2*x2\n   until_p x1>=w1-d;\n   // L0\n   x1'=-a1*x1+b1*x2,\n   x2'=b2*x1-a2*x2\n   until_p x1<=80+d && x2<=80+d;\n   // L2\n   x1'=-a1*x1+b1*x2,\n   x2'=b2*x1-a2*x2-(k21_l2*x1+k22_l2*x2) + (k21_l2*w1+k22_l2*w2)\n   until_p x2>=w2-d;\n   // L0\n   x1'=-a1*x1+b1*x2,\n   x2'=b2*x1-a2*x2\n   until_p x1<=80+d && x2<=80+d;\n   i++;\n}\n---\nvars x1, x2, i\nuntil 1000\niterations 500"
      -> "Variation of a system of two heated tanks from the literature.",
    // "Water tank"
    //   -> "a1:=1; //Area of tank 1\na2:=1; // Area of tank 2\nr1:=1; //Resistance applied to the water flow at the water exit tap of tank 1.\nr2:=10; //Resistance applied to the water flow at the water exit tap of tank 2.\nh1_p:=10; // initial water level of tank 1 in the aligned  configuration\nh2_p:=0; // initial water level of tank 2 in the aligned  configuration.\nh1_v:=10; //initial water level of tank 1 in vertical configuration.\nh2_v:=0; //initial water level of tank 2 in vertical  configuration.\n\n// Open tap of the tank 1 and close the tap of the tank 2\nqe1:=1;\nqe2:=0;\n\n// Differential equations simulating the variation of the water level in the two tanks, in each configuration, after 50 seconds.\nh1_p'=-pow(a1*r1,-1)*h1_p+pow(a1*r1,-1)*h2_p+pow(a1,-1)*qe1,\nh2_p'=pow(a2*r1,-1)*h1_p-pow(a2*r1,-1)*h2_p+pow(a2,-1)*qe2-pow(a2*r2,-1)*h2_p,\nh1_v'=-pow(a1*r1,-1)*h1_v+pow(a1,-1)*qe1,\nh2_v'=pow(a2*r1,-1)*h1_v-pow(r2*a2,-1)*h2_v + pow(a2,-1)*qe2 for 40;\n\n// Open tap of the tank 2 and close the tap of the tank 1\nqe1:=0;\nqe2:=1;\n\n// Differential equations simulating the variation of the water level in the two tanks, in each configuration, after 50 seconds.\nh1_p'=-pow(a1*r1,-1)*h1_p+pow(a1*r1,-1)*h2_p+pow(a1,-1)*qe1,\nh2_p'=pow(a2*r1,-1)*h1_p-pow(a2*r1,-1)*h2_p+pow(a2,-1)*qe2-pow(a2*r2,-1)*h2_p,\nh1_v'=-pow(a1*r1,-1)*h1_v+pow(a1,-1)*qe1,\nh2_v'=pow(a2*r1,-1)*h1_v-pow(r2*a2,-1)*h2_v + pow(a2,-1)*qe2 for 40;\n\n//Open both\nqe1:=1;\nqe2:=1;\n\n// Differential equations simulating the variation of the water level in the two tanks, in each configuration, after 50 seconds.\nh1_p'=-pow(a1*r1,-1)*h1_p+pow(a1*r1,-1)*h2_p+pow(a1,-1)*qe1,\nh2_p'=pow(a2*r1,-1)*h1_p-pow(a2*r1,-1)*h2_p+pow(a2,-1)*qe2-pow(a2*r2,-1)*h2_p,\nh1_v'=-pow(a1*r1,-1)*h1_v+pow(a1,-1)*qe1,\nh2_v'=pow(a2*r1,-1)*h1_v-pow(r2*a2,-1)*h2_v + pow(a2,-1)*qe2 for 40;\n\n//Close both\nqe1:=0;\nqe2:=0;\n\n// Differential equations simulating the variation of the water level in the two tanks, in each configuration, after 50 seconds.\nh1_p'=-pow(a1*r1,-1)*h1_p+pow(a1*r1,-1)*h2_p+pow(a1,-1)*qe1,\nh2_p'=pow(a2*r1,-1)*h1_p-pow(a2*r1,-1)*h2_p+pow(a2,-1)*qe2-pow(a2*r2,-1)*h2_p,\nh1_v'=-pow(a1*r1,-1)*h1_v+pow(a1,-1)*qe1,\nh2_v'=pow(a2*r1,-1)*h1_v-pow(r2*a2,-1)*h2_v + pow(a2,-1)*qe2 for 40;\n---\nuntil 100\nsamples 60\nvars h.*",
  )

  val configs: List[Example] = List(
    "Simple portrait"
      -> "// Solution not naively computed (precise solution involves sin/cos)\n// Use the online version to use the precise solution.\np:=1;v:=1;\np'=v, v'=-p for 4;\n--\nportrait p,v\nuntil 4"
      -> "Simple portait of two waves.",
    "CC portrait"
      -> "// Cruise control (2D)\nx:=0; y:=0;\nvx:=0; vy:=20;\nax:=5; ay:=-2;\nwhile true do {\n  if vx<=10 then ax:=5; else ax:=-2;\n  if vy<=0 then ay:=5; else ay:=-2;\n  x'=vx,vx'=ax,\n  y'=vy,vy'=ay for 1;\n}\n--\nportrait vx,vy; x,y\nuntil 15"
      -> "Portrait of the cruise control example, targetting a x-velocity of 10 and a y-velocity of 0.",
    "Pursue portrait"
      -> "// Pursuit (2D)\n// Initial position and velocity of the missile\nx:=300; vx:=20;\ny:=300; vy:=0;\n// Initial position and velocity of the target\nxl:=500; vxl:=15;\nyl:=500; vyl:=0;\n\n// Angular velocity of the missile\naw:=(1/20)*2*pi();\n// Angular velocity of the target\nawl:=(1/40)*2*pi();\n\n// Counter\ncont:=0;\n// Decision time\nsampling_time:=0.1;\n// Minimum collision distance\ndist_min_col:=1; \n// variable that stores the alpha angle\nalpha:=0;\n//Variable that stores the vectorial product to decide which way to turn\nvect_P:=0;\n// Variables that stores the angular velocity decision to the missile and the target\nw:=0;\nwl:=0;\n//Variables that stores the relative positions and velocities\ndx:=0;\ndy:=0;\nvrelx:=0;\nvrely:=0;\n\n// Run the following programme whilst the distance between the missile and the target is greater than \n//the collision distance\nwhile (sqrt((x-xl)^2+(y-yl)^2)>dist_min_col) do {\n    //Conditional structures to establish the target path\n    if (cont<=100)\n    then wl:=0;\n    else {\n          if (cont<=200)\n          then wl:=-awl;\n          else {\n              if (cont<=300)\n              then wl:=awl;\n              else wl:=0;\n              }\n        }\n    // The counter is incremented\n    cont:=cont+1;\n    //Update distances and relative velocities\n    dx:=xl-x;\n    dy:=yl-y;\n    vrelx:=vxl-vx;\n    vrely:=vyl-vy;\n    // Determine the value of the angle alpha\n    alpha:=arccos((vrelx*dx + vrely*dy)/(sqrt(vrelx^2 + vrely^2)*sqrt(dx^2 + dy^2))); \n    // Conditional structures to determine whether the missile needs to move forward or make a curve\n    if (alpha>=179.5*pi()/180 && alpha<=180.5*pi()/180)\n    then {\n        // If the theta is  between 179.5 and 180.5 degrees, the missile follows a straight line at a constant velocity \n        w:=0;\n        }\n    else {\n        // Determine the value of the vetorial product between the relative velocity vector and the relative position vector\n        vect_P:=vrelx*dy-vrely*dx;\n        // If the theta is not between 179.5 and 180.5 degrees, the missile needs to curve to the left or right\n        // To decide which way to turn, simply check the sign of the vectorial product. \n        if (vect_P>=0)\n        then {\n              // If the vectorial product is positive or zero,  it curves to the right\n              w:=aw;\n              }\n          else {\n              // If the vectorial product is negative,  it curves to the left\n              w:=-aw;\n              }\n        }\n    // Differential equations\n    x'=vx,y'=vy,vx'=w*vy,vy'=-w*vx,\n    xl'=vxl,yl'=vyl,vxl'=wl*vyl,vyl'=-wl*vxl for sampling_time;\n}\n--\nportrait x,y; xl,yl\niterations 1000\nuntil 50\n"
      -> "Pursuit of a leader at (xl,yl) by an object at (x,y). The pursuer could be a missile and the leader could be a target. The pursuer tries to reach the target by adjusting its angular velocity (w) based on the relative position and velocity of the target. The target follows a predefined path with its own angular velocity (wl).",
    "[Macros] Adaptive CC"
      -> "// Adaptive Cruise Control (ACC)\ndef fwd:=3; // constant\ndef bwd:=-3; // constant\n\n@keep def als := [1,2,3,0,-1,-2,-3];\npl :=50; vl := 0;\nal:= als; //leader \npf := 0 ; vf := 0; af := fwd;  //follower\nst := 2; //sample time\n\n// Syntactic Macros\ndef bt := (al-fwd)*st+vl-vf;\ndef at := (al-bwd)/2; \ndef ct := (((al-fwd)/2)*st^2+(vl-vf)*st+pl-pf);\ndef discr := bt^2 - 4*at*ct;\ndef safe :=  ct<=0 || \n   (at==0 && bt!=0 && -ct/bt > 0) || \n    (discr >= 0 && at!=0 &&\n     ((-bt - sqrt(discr))/(2*at) > 0  ||\n      (-bt + sqrt(discr))/(2*at) > 0 ));\n\nwhile true do {\n if safe\n then af :=bwd;  //brake \n else af :=fwd; //accelerate   \n//update states\n  pf'=vf, vf'=af,\n  pl'=vl, vl'=al for st;}\n---\nvars pl,pf\nuntil 15\n//runs 7\n"
      -> "Adaptive cruise control - experiment using <code>def</code> macros.",
    "[Macros] Counter"
      -> "// loop that ends when a stream terminates\ndef counter:=[1,...,5];\nx:=0;\nwhile counter { // means \"while counter!=0\"\n  wait 1;\n  x := x+1;\n}\n---\nuntil 10"
      -> "Example that stops once a given stream of values reaches the end.",
    "[Macros] keeping streams"
      -> "// Example illustrating a stream \"kept\" that is kept\n// (i.e., cannot be redefined) and one \"redf\" that\n// can be redefined. This also holds for multiple runs.\n\n@keep def kept := [1,2,3,4];\n      def redf := [11,12,13,14];\nx := kept; // should pop value 1 (in the first run)\ny := redf; // should pop value 11 (always)\nwait 1;\n\n@keep def kept := [5,6,7,8]; // should be ignored\n		  def redf := [20,21,22,23,24];\nx := kept; // should pop value 2 (in the first run)\ny := redf; // should pop value 21 (always)\nwait 1;\n\n--\nruns 3\n// when running the second run, x becomes 3 and 4;\n// when running the thrird run, it will stop when\n//   trying to pop from kept in line 3."
      -> "Example illustrating a stream \"kept\", marked with @keep, that cannot be redefined (even accross multiple runs). To compare, a normal stream \"redf\" can be redefined - when running multiple times it will always produce the same results.",
    "Sin(x)"
      -> "x:=0; y:=0;\n// Simulating sin(x) using ODEs\nwhile true\n  x'=1, y'=cos(x) for 5;\n---\nuntil 20\nrk-samples 2 // change to 1 to drop precision\nsamples 100  // change to 20 to view less points\nverbose\nvars y\n"
      -> "Simulating sin(x) using ODEs. Change the <code>rk-samples</code> and the (plot) <code>samples</code> to experiment with configurations of the simulation.",
    "Precision test"
      -> "theta := 0.1;\na := 0;\ntheta' = a,\n    a' = sin(theta) for 120;\n\n---\nuntil 30\nsamples 80\nrk-samples 18 // increase (100) to fix example\n"
      -> "Example with a solution that requires enough precision to be computed correctly. Increase the number of <code>rk-samples</code> (used in the Runge Kutta), e.g., to <code>100</code>, to improve precision.",
    // "Simple cruise control 3d"
    //   -> "// Cruise control (3D)\nx:=0; y:=0; z:=0;\nvx:=0; vy:=20; vz:=10;\nax:=5; ay:=-2; az:=-1;\nwhile true do {\n  if vx<=10 then ax:=5; else ax:=-2;\n  if vy<=0 then ay:=5; else ay:=-2;\n  if vz<=0 then az:=1; else az:=-1;\n  x'=vx,vx'=ax,\n  y'=vy,vy'=ay,\n  z'=vz,vz'=az for 1;\n}\n--\nportrait vx,vy,vz; x,y,z\nuntil 15"
    //   -> "Variation of the cruise control example, with an additional dimension (z) and a target z-velocity of 0.",

  )

  val prob: List[Example] = List(
    "PPDP - Ex.2.1"
      -> "x := 0 ; c := 0 ;\nwhile c <= 5 do {\n  bernoulli (1/2)\n    x++; x--;\n  c++;\n} \nx := x/sqrt(5);"
      -> "Example 2.1 - An execution sample of a random walk with 50% chances of increasing or decreasing a variable x.",
    "PPDP - Ex.2.2"
      -> "x := 0;\nwhile true {\n  bernoulli (1/2)\n    x++; x--;\n  wait unif(0,1);\n}"
      -> "Example 2.2 - An execution sample of a continuous-time random walk in which the waiting time is given by sampling from the uniform distribution on [0,1].",
    "PPDP - Ex.2.3"
      -> "d:=0;\np := 10 ; v := 0 ;\nwhile true {\n  d := unif (2,4) ;\n  p'=v,v'= -9.8 for d ;\n  v := -v;\n}"
      -> "Example 2.3 - An execution sample of the ball's position (p) and velocity (v) during the first 5 time units.",
    "PPDP - Ex.2.4"
      -> "lambda:=2; d:=0;\n// the \"seed\" variable fixes\n// the pseudo-random generator\np:=0; v:=0; a:=0;\nwhile true {\n  d:=expn(lambda);\n  bernoulli (1/2)\n    a--; a++;\n  p'= v, v'=a for d;\n}\n---\nruns 20\nuntil 15\nvars p"
      -> "Example 2.4 - Multiple execution samples of the particle’s position overlayed, in order to depict how the position’s probability mass spreads over space w.r.t time.",
    "PPDP - Ex.2.5"
      -> "x:=expn(2); y:=expn(2);\np:=0; v:=0;\np'=v, v'=1  for sqrt (3) + x;\np'=v, v'=-1 for sqrt (3) + y;"
      -> "Example 2.5 - Execution sample of a particle's position (p) and velocity (v).",
    "PPDP - Ex.2.6a"
      -> "// Adaptive Cruise Control (ACC)\np:=0; v:=0;  // follower  \npl:=50; vl:=10; // leader\na:=0;\nwhile true {\n  // decide to speed up (acc=2) or brake (acc=-2)\n  if (v-8)^2 + 4*(p-pl+v-9) < 0\n  then p'=v, v'= 2, pl'=vl, vl'=a for 1;\n  else p'=v, v'=-2, pl'=vl, vl'=a for 1;\n}\n----\nuntil 20\nvars p.*"
      -> "Example 2.6a - Adaptive cruise control with a leader with constant acceleration (no uncertainty).",
    "PPDP - Ex.2.6b"
      -> "// Adaptive Cruise Control (ACC)\np:=0; v:=0;  // follower  \npl:=50; vl:=10; // leader\na:=0;\nwhile true {\n  a := unif(-1,1) ;\n  // decide to speed up (acc=2) or brake (acc=-2), assuming a==-1\n  if (v - vl + 3)^2 + 4*(p - pl + v - vl + 3/2) < 0\n  // uncomment the \"if\" below to try version 2.6a (incorrect)\n  // if (v-8)^2 + 4*(p-pl+v-9) < 0\n  then p'=v, v'= 2, pl'=vl, vl'=a for 1;\n  else p'=v, v'=-2, pl'=vl, vl'=a for 1;\n}\n----\nuntil 20\nseed 10\nvars p.*"
      -> "Example 2.6b Adaptive cruise control with a leader with an uncertain acceleration (bounded by fixed values).",
    "PPDP - Ex.2.6c"
      -> "// Adaptive Cruise Control (ACC)\n// (unexpected delay)\np := 0; v := 0; // follower  \npl:=50; vl:=10; // leader\nlambda:=2;\nwhile true {\n  x := expn(lambda) ; x++ ;\n  // decide to speed up (acc=2) or brake (acc=-2)\n  if (v-8)^2 + 4*(p-pl+v-9) < 0\n  then p'=v, v'= 2, pl'=vl, vl'=0 for x;\n  else p'=v, v'=-2, pl'=vl, vl'=0 for x;\n}\n----\nuntil 20\nvars p.*"
      -> "Example 2.6c: Adaptive cruise control with a leader with an uncertain delay during rounds (unbounded, given by a exponential distribution).",
    "RLC (probabilistic)"
      -> "volt:=0; d:=0; v:=0;\nc:=0.047; l:=0.047;\nres:=0.5+unif(-0.2,0.2);\n\nwhile true do {\n  if (volt<10) then v:=18;\n               else v:=0;\n  volt'=d,\n  d'=-(d*res/l)\n      -volt/(l*c)+v/(l*c)\n  for 0.01;\n}\n--\nuntil 0.6\nvars volt\nruns 10"
      -> "Variation of the RLC circuit example with a probabilistic resistance, simulated 10x.",
    "Normal-dist"
      -> "while true {\n  // normal dist. mean 5, s-dev 1:\n  d:=normal(5,1);\n\n	// try also others:\n  // negative dist. mean 2:\n  // d:=expn(1/2);\n	// powerlaw distribution with exponent 2 and min 1\n  //d:=powerlaw(2,1);\n	wait 1;\n}\n---\nruns 50"
      -> "Sampling from a normal distribution (or other).",
    // "Powerlaw-dist"
    //   -> "while true {\n	// powerlaw distribution with exponent 2 and min 1\n  d:=powerlaw(2,1);\n  // try also a negative dist. mean 2:\n  //d:=expn(1/2);\n  // try also a normal dist. mean 5, s-dev 1:\n  //d:=normal(5,1);\n	wait 1;\n}\n---\nruns 50"
    //   -> "Sampling from a powerlaw distribution (or other).",
  )
