package lince.syntax

import lince.syntax.Lince.*
import lince.backend.Stream
import lince.backend.Stream.{ExprStrm,ListStrm,SeqStrm,RandomStrm}

/**
 * List of functions to produce textual representations of commands
 */
object Show:

  def apply(p: Program): String = p match
    case Program.Skip => "skip; "
    case Program.Assign(v, e) => s"$v:=${apply(e)}; "
    case Program.StreamDef(v, s) => s"def $v:=${Show(s)}; "
    case Program.EqDiff(eqs, dur) if eqs.isEmpty =>
       s"{} for ${dur.map(apply).getOrElse("forever")}; "
    case Program.EqDiff(eqs, dur) =>
      eqs.map(kv => s"${kv._1}'=${apply(kv._2)}").mkString(", ") +
                    s" for ${dur.map(apply).getOrElse("forever")}; "
    case Program.Seq(Program.Skip, q) => apply(q)
    case Program.Seq(Program.Seq(p,q),r) => apply(Program.Seq(p,Program.Seq(q,r)))
    case Program.Seq(p, q) => apply(p)+"\n"+apply(q)
    case Program.ITE(b, pt, Program.Skip) => s"if ${apply(b)}:\n${ind(apply(pt))}"
    case Program.ITE(b, pt, pf) => s"if ${apply(b)}:\n${ind(apply(pt))}\nelse\n${ind(apply(pf))}"
    case Program.While(b, p) => s"while ${apply(b)}:\n${ind(apply(p))}"

  def ind(s:String,i:Int=3) = (" "*i)+s.replaceAll("\n",s"\n${" "*i}")

  def apply(e: Expr): String = e match
    case Expr.Num(n) => n.toString
    case Expr.True => "true"
    case Expr.False => "false"
    case Expr.Var(x) => x
    case Expr.Func(op, es) if "+-/*^<>=|&".contains(op.headOption.getOrElse(' ')) =>
      es.map(applyP).mkString(s"$op")
    case Expr.Func("!", List(e)) => s"!${applyP(e)}"
    case Expr.Func(op, es) =>
      s"$op(${es.map(apply).mkString(", ")})"

  def apply(s:Stream): String = s match
    case ExprStrm(e,k) => apply(e)+keep(s)
    case ListStrm(l,k) => l.mkString("[",",","]")+keep(s)
    case SeqStrm(from,to,by,k) => s"[$from,..,${to.map(_.toString).getOrElse("inf")} by $by]"+keep(s)
    case RandomStrm(seed,k) => s"${seed % 1000}${if seed<1000 && seed> -1000 then "" else ".."}"//+keep(s)
  private def keep(s:Stream): String = if s.keep then "@k" else ""
  

  def applyP(e: Expr): String = e match
    case Expr.Func(_,es) if es.size>1 => s"(${apply(e)})"
    case _ => apply(e)

  def apply(a:Action): String = a match {
    case Action.Assign(v, n) => s"$v:=$n"
    case Action.StrmDef(v, s) => s"def $v:=${Show(s)}"
    case Action.DiffStop(eqs, time) => s"diff-stop@$time"
    case Action.DiffSkip(eqs, time) => s"diff-skip@$time"
    case Action.CheckIf(b, true) => s"if-true: ${apply(b)}"
    case Action.CheckIf(b, false) => s"if-false: ${apply(b)}"
    case Action.CheckWhile(b, true) => s"wh-true: ${apply(b)}"
    case Action.CheckWhile(b, false) => s"wh-false: ${apply(b)}"
  }

  def simpleStatm(p: Program): String = p match {
    case Program.Seq(Program.Seq(p1,p2), p3) => simpleStatm(Program.Seq(p1,Program.Seq(p2,p3)))
    case Program.Seq(Program.Skip, p2) => /*"skip; "+*/simpleStatm(p2)
    case Program.Seq(p1, p2) => simpleStatm(p1)+"..."
    case Program.While(b, p2) => s"while ${apply(b)} {...}"
    case Program.ITE(b, pt, pf) => s"if ${apply(b)} {...} {...}"
    case _ => apply(p)
  }

  def simpleSt(st: lince.backend.SmallStep.St): String =
    s"[${st.t}/${st.lp}] {${st.s.map(x=>s"${x._1}:${Show(x._2)}").mkString(",")}} {${st._2.mkString(",")}} ${simpleStatm(st._1)}"
    //s"[${st.t}/${st.lp}/${st.s}] {${st._2.mkString(",")}} ${simpleStatm(st._1)}"

  def simpleStML(st: lince.backend.SmallStep.St): String =
    s"[${st.t}/${st.lp}] {${st.s.map(x=>s"${x._1}:${Show(x._2)}").mkString(",")}} {${st._2.mkString(",")}}\n${simpleStatm(st._1)}"

