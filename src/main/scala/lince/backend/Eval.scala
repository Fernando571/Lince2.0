package lince.backend

import lince.syntax.{Lince, Show}
import Lince.Expr
import lince.backend.Stream
import Stream.{Streams,RandomStrm,SeqStrm,ListStrm,ExprStrm}


import scala.util.Random

object Eval:

  //  type Valuation = Map[String,Expr]
  type Valuation = Map[String,Double]
  private type MValuation = scala.collection.Map[String,Double]

  def asBoolean(e:Expr)(using v:MValuation): Boolean =
    apply(e)(using v) match
      case b: Boolean => b
      case d: Double => d != 0

  def asDouble(e:Expr)(using v:MValuation): Double =
    apply(e)(using v) match
      case b: Boolean => sys.error(s"Expected real, but found a boolean, at ${Show(e)}")
      case d: Double => d

  def apply(e:Expr)(using v:MValuation): Double | Boolean = e match
    case Expr.Num(n) => n
    case Expr.True   => true
    case Expr.False  => false
    case Expr.Var(x) => v.getOrElse(x,
        sys.error(s"[Eval] Variable $x not found - only ${v.keys.mkString(",")}"))
    // Real functions
    case Expr.Func("+", List(e1,e2)) => asDouble(e1) + asDouble(e2)
    case Expr.Func("-", List(e1,e2)) => asDouble(e1) - asDouble(e2)
    case Expr.Func("*", List(e1,e2)) => asDouble(e1) * asDouble(e2)
    case Expr.Func("/", List(e1,e2)) => asDouble(e1) / asDouble(e2)
    case Expr.Func("^", List(e1,e2)) => math.pow(asDouble(e1),asDouble(e2))
    case Expr.Func("pow", List(e1,e2)) => math.pow(asDouble(e1),asDouble(e2))
    case Expr.Func("sqrt", List(e)) => math.sqrt(asDouble(e))
    case Expr.Func("exp", List(e)) => math.exp(asDouble(e))
    case Expr.Func("round",List(e1)) => math.round(asDouble(e1)).toDouble
    case Expr.Func("sin", List(e)) => math.sin(asDouble(e))
    case Expr.Func("cos", List(e)) => math.cos(asDouble(e))
    case Expr.Func("tan", List(e)) => math.tan(asDouble(e))
    case Expr.Func("cosh", List(e)) => math.cosh(asDouble(e))
    case Expr.Func("sinh", List(e)) => math.sinh(asDouble(e))
    case Expr.Func("tanh", List(e)) => math.tanh(asDouble(e))
    case Expr.Func("arccos", List(e)) => math.acos(asDouble(e))
    case Expr.Func("arcsin", List(e)) => math.asin(asDouble(e))
    case Expr.Func("ln", List(e)) => math.log(asDouble(e))
    case Expr.Func("pi", List()) => math.Pi
    // Booleans functions
    case Expr.Func("&&",l) => l.map(asBoolean).forall(x=>x)
    case Expr.Func("||",l) => l.map(asBoolean).exists(x=>x)
    case Expr.Func("!",List(e)) => !asBoolean(e)
    case Expr.Func("==", List(e1, e2)) => apply(e1) == apply(e2)
    case Expr.Func("!=", List(e1, e2)) => apply(e1) != apply(e2)
    case Expr.Func(">=", List(e1, e2)) => asDouble(e1) >= asDouble(e2)
    case Expr.Func("<=", List(e1, e2)) => asDouble(e1) <= asDouble(e2)
    case Expr.Func(">", List(e1, e2)) => asDouble(e1) > asDouble(e2)
    case Expr.Func("<", List(e1, e2)) => asDouble(e1) < asDouble(e2)
    case Expr.Func(op, es) =>
      sys.error(s"[Eval] Cannot evaluate function ${Show(e)}")

  // def apply(c:Cond)(using v:Valuation): Boolean = c match
  //   case Cond.True => true
  //   case Cond.False => false
  //   case Cond.And(c1, c2) => apply(c1) && apply(c2)
  //   case Cond.Or(c1, c2) => apply(c1) || apply(c2)
  //   case Cond.Not(c1) => !apply(c1)
  //   case Cond.Comp("==", e1, e2) => apply(e1) == apply(e2)
  //   case Cond.Comp("!=", e1, e2) => apply(e1) != apply(e2)
  //   case Cond.Comp(">=", e1, e2) => apply(e1) >= apply(e2)
  //   case Cond.Comp("<=", e1, e2) => apply(e1) <= apply(e2)
  //   case Cond.Comp(">", e1, e2) => apply(e1) > apply(e2)
  //   case Cond.Comp("<", e1, e2) => apply(e1) < apply(e2)
  //   case Cond.Comp(op, e1, e2) =>
  //     sys.error(s"Cannot evaluate condition ${Show(c)}")

  // def rands(e:Expr)(using v:MValuation): Expr = e match {
  //   case Expr.Num(n) => e
  //   case Expr.True   => e
  //   case Expr.False  => e
  //   case Expr.Var(x) => e
  //   case Expr.Func("unif",List()) => Expr.Num(rand.nextDouble())
  //   case Expr.Func("unif",List(Expr.Num(n1),Expr.Num(n2))) => Expr.Num(rand.between(n1,n2))
  //   case Expr.Func("unif",_) => sys.error(s"Cannot evaluate a random function with variables here (${Show(e)})")
  //   case Expr.Func(n,es) => Expr.Func(n,es.map(rands))
  // }

  def apply(e:Expr,ss:Streams)(using v:MValuation)
        : Option[(Double | Boolean , Streams)] =
    evalStreams(e,ss).map((e,ss2) => (apply(e),ss2))

  def asBoolean(e:Expr,ss:Streams)(using v:MValuation)
        : Option[(Boolean , Streams)] =
    evalStreams(e,ss).map((e,ss2) => (asBoolean(e),ss2))

  def asDouble(e:Expr,ss:Streams)(using v:MValuation)
        : Option[(Double , Streams)] =
    evalStreams(e,ss).map((e,ss2) => (asDouble(e),ss2))


  // def evalStreams(e:Expr, ss:Strms): Option[(Expr,Strms)] =
  //   val res = evalStreamsTMP(e, ss)
  //   println(s"[DEBUG] ${Show(e)} ===> ${Show(res.getOrElse((e,ss))._1)} (knowing ${ss.keys.mkString(",")})")
  //   res

  def evalStreams(e:Expr, ss:Streams): Option[(Expr,Streams)] = e match
    case Expr.Var(x:String) if ss contains x => ss(x).pop match
      case Some((e2,s2)) =>
        evalStreams(e2, ss-x).map((e3,ss3) => (e3,ss3+(x->s2)))
      case None => None
    case Expr.Func(op:String, Nil) if ss contains op =>
      evalStreams(Expr.Var(op),ss)
    case Expr.Func(op:String, es:List[Expr]) =>
      var newss = ss-op
      var stop = false
      val newes = for e <- es yield
        evalStreams(e, newss) match
          case None =>
            stop = true
            e
          case Some(e2,ss2) =>
            newss = ss2 
            e2
      if stop then None else Some(Expr.Func(op,newes) -> newss)
    case _ => Some((e,ss))
  
