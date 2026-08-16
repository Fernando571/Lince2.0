package lince.backend

import lince.backend.Eval.Valuation
import lince.syntax.Lince.Expr
import scala.collection.mutable.{Map => MMap}

object RungeKutta:

  /**
    *
    * @param input Initial values of the variables
    * @param eqs Differential equations
    * @param time time to perform the Diff.eqs
    * @return Assignment of each variable to a value at the given time
    */
  def apply(input:Valuation, eqs:Map[String,Expr], time:Double, numSteps:Int=100): Valuation  = {

    val stepValuation = MMap[String, Double]()
    stepValuation ++= input //.map(kv=>(kv._1->Eval(kv._2)(using Map())))

//    val h:Double=0.001 //step size
//    val numSteps:Int=(time/h).toInt //number of steps until the 'time'
    // idea: to avoid taking too long when there are many rounds with ODEs, we can
    // count how many ODEs were already evaluated, and give less and less precision (discount factor).
    // val numSteps:Int=500 //number of steps until the 'time'
    val h:Double=time/numSteps //step size
    val accum:MMap[String,Double]=stepValuation.clone() //Map to perform the formulation of runge-kutta
    val k1 = MMap.empty[String,Double].withDefaultValue(0)
    val k2 = MMap.empty[String,Double].withDefaultValue(0)
    val k3 = MMap.empty[String,Double].withDefaultValue(0)
    val k4 = MMap.empty[String,Double].withDefaultValue(0)

    for (i <- 0 until numSteps){

      // Determination of k1 for all differential equations
      for ((v,exp) <- eqs)
        k1(v)=h*Eval.asDouble(exp)(using accum)

      // Determination of k2 for all differential equations
      for ((key, value) <- accum)
        accum(key) = stepValuation(key)+k1(key)/2
      for ((v,exp) <- eqs)
        k2(v)=h*Eval.asDouble(exp)(using accum)

      // Determination  k3 for all differential equations
      for ((key, value) <- accum)
        accum(key) = stepValuation(key)+k2(key)/2
      for ((v, exp) <- eqs)
        k3(v) = h * Eval.asDouble(exp)(using accum)

      // Determination of k4 for all differential equations
      for ((key, value) <- accum)
        accum(key) = stepValuation(key)+k3(key)
      //println("iteration_4:",accum)
      for ((v, exp) <- eqs)
        k4(v) = h * Eval.asDouble(exp)(using accum)

      //Update stepValuation
      for ((key, value) <- stepValuation)
        stepValuation(key) = value + (k1(key) + 2*k2(key) + 2*k3(key) + k4(key))/6
    }

    //return valuation at the end
    stepValuation.toMap
  }

