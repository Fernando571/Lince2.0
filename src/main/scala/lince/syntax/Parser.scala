package lince.syntax

import cats.parse.Numbers.digits
import cats.parse.Parser.*
import cats.parse.{LocationMap, Parser as P, Parser0 as P0}
import lince.syntax.Lince.{Expr, PlotInfo, Program, Simulation}
import Program.*
import lince.backend.Stream
import Stream.{Streams,ListStrm,SeqStrm,ExprStrm}
import caos.frontend.widgets.WidgetInfo.Simulate

import scala.sys.{env, error}
import scala.util.Random

object Parser :

  /** Parse a command  */
  def parseProgram(str:String):Program =
    pp(program,str) match {
      case Left(e) => error(e)
      case Right(c) => c
    }

  def parseSimulation(str: String): Simulation =
    pp(simulation, str) match {
      case Left(e) => error(e)
      case Right(c) => c
    }


  /** Applies a parser to a string, and prettifies the error message */
  private def pp[A](parser:P[A], str:String): Either[String,A] =
    parser.parseAll(str) match
      case Left(e) => Left(prettyError(str,e))
      case Right(x) => Right(x)

  /** Prettifies an error message */
  private def prettyError(str:String, err:Error): String =
    val loc = LocationMap(str)
    val pos = loc.toLineCol(err.failedAtOffset) match
      case Some((x,y)) =>
        s"""at (${x+1},$y):
           |<pre>${loc.getLine(x).getOrElse("-")}</br>${("-" * y)+"^\n"}</pre>""".stripMargin
      case _ => ""
    s"${pos}expected: ${err.expected.toList.mkString(", ")}\noffsets: ${
      err.failedAtOffset};${err.offsets.toList.mkString(",")}"

  // Simple parsers for spaces and comments
  /** Parser for a sequence of spaces or comments */
  private val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  private val comment: P[Unit] = string("//") *> P.charWhere(_!='\n').rep0.void
  private val sps: P0[Unit] = (whitespace | comment).rep0.void

  // Parsing smaller tokens
  private def alphaDigit: P[Char] =
    P.charIn('A' to 'Z') | P.charIn('a' to 'z') | P.charIn('0' to '9') | P.charIn('_')
  private def varName: P[String] =
    (charIn('a' to 'z') ~ alphaDigit.rep0).string
  private def procName: P[String] =
    (charIn('A' to 'Z') ~ alphaDigit.rep0).string
  private def symbols: P[String] =
    // symbols starting with "--" are meant for syntactic sugar of arrows, and ignored as symbols of terms
    P.not(string("--")).with1 *>
    oneOf("+-><!%/*=|&".toList.map(char)).rep.string
  private def regExp: P[List[String]] =
    (alphaDigit | charIn("*. ()|+[]^!")).rep.string.map(_.trim).repSep(char(',')*>sps).map(_.toList)
//      .map(x => List(x))
  /** real number, e.g., 12 or 34.2 */
  def realP: P[Double] =
    (digits ~ (charIn('.')*>digits.map("."+_)).?)
      .map(x=>(x._1+x._2.getOrElse("")).toDouble)
  def realnP: P[Double] =
    (char('-').?.with1 ~ (digits ~ (charIn('.')*>digits.map("."+_)).?))
      .map(x=>(x._2._1+x._2._2.getOrElse("")).toDouble * (if x._1.isDefined then -1 else 1))
  /** Positive integer */
  def intP: P[Int] = digits.map(_.toInt)

  //import scala.language.postfixOps

  private def simulation: P[Simulation] =
//    (sps.with1 *> program ~ plotInfo.? <* sps).map{
    (program ~ plotInfo.? <* sps).map {
        case (p,Some(pi)) => Simulation(p,pi)
        case (p,None) => Simulation(p, PlotInfo.default)
    }

  /** A program is a command with possible spaces or comments around. */
  private def program: P[Program] =
    statement.surroundedBy(sps).rep.map(l => l.tail.fold(l.head)(Program.Seq.apply))
//    (statement <* sps).rep.map(l => l.tail.fold(l.head)(Program.Seq.apply))

  private def block(rec: P[Program]): P[Program] =
    (char('{') *> sps *> (rec <* sps).rep0 <* char('}')).map(l =>
      if l.isEmpty then Skip else l.tail.fold(l.head)(Program.Seq.apply))

  private def statement: P[Program] = P.recursive((recSt: P[Program]) => {
    skip |
    ite(recSt) |
    whileP(recSt) |
    bern(recSt) |
    block(recSt) |
    waitP |
    strmDef |
    ((varName <* sps) ~ (assign | diffEq | suffix) ).map (x => x._2 (x._1) )
  })

  def skip: P[Program] =
    (string ("skip") *> sps *> char (';') ).as(Skip)

  def ite(rec:P[Program]): P[Program] =
    ((string("if") *> sps *> cond ~
      (sps *> (string("then") *> sps).? *> // optional "then"
      rec <* sps)) ~
    (string("else") *> sps *> rec).?)
      .map(x => ITE(x._1._1,x._1._2,x._2.getOrElse(Skip)))

  def bern(rec: P[Program]): P[Program] =
    ((string("bernoulli") *> sps *> expr) ~
      (sps *> rec <* sps) ~
      rec)
      .map(x => ITE(Expr.Func("<",List(Expr.Func("unif",Nil),x._1._1)),
                    x._1._2, x._2))

  def whileP(rec:P[Program]): P[Program] =
    (string("while") *> sps *> cond ~
      (sps *> (string("do") *> sps).? *> // optional do
        rec <* sps))
      .map(x => While(x._1, x._2)) |
    (string("repeat") *> sps *> intP ~ (sps *> rec <* sps))
      .map(x => Seq(
        Assign("§c",Expr.Num(0)),
        While(Expr.Func("<",List(Expr.Var("§c"),Expr.Num(x._1))),
          Seq(x._2,Assign("§c",Expr.Func("+",List(Expr.Var("§c"),Expr.Num(1))))))))

  def waitP: P[Program] =
    string("wait") *> sps *> expr.map(e => EqDiff(Map(),Some(e))) <* sps <* char(';')

  def strmDef: P[Program] =
    ((string("@keep") *> sps).?.with1 ~
     (string("def") *> sps *> (varName <* sps <* string(":=") <* sps) ~ stream))
      .map(res =>
        val strm = res._2._2(res._1.isDefined)
        StreamDef(res._2._1,strm)) 

  def stream: P[Boolean => Stream] =
    // (char('[') *> realnP.repSep(sps *> char(',') *> sps) <* char(']') <* sps <* char(';'))
    //   .map(x => ListStrm(x.toList,false)) |
    (char('[') *> sps *> (seqOrList <* sps <* char(';'))) |
    (expr <* sps <* char(';'))
      .map(e => kp => ExprStrm(e,kp))

  // parses either:
  //   a real number,
  //   a list of real numbers,
  //   a range of real numbers (e.g., `1.0,...,5.0`),
  //   a range of real numbers of real numbers with a step size (e.g., `1.0,1.5,...,5.0`), or
  //   an open-ended range of real numbers (e.g., `1.0,...` or `1.0,1.5,...`).
  def seqOrList: P[Boolean => Stream] =
    (char(']')).as(ListStrm(Nil,_)) |
    (realnP.repSep0(sps *> char(',') *> sps).with1 <* char(']'))
      .map(x => ListStrm(x.toList,_)).backtrack |
    (realnP ~ (sps *> char(',') *> sps *> string("...") *> sps *>
      (char(',') *> sps *> realnP).?) <* char(']'))
      .map{
        case (from,to) => SeqStrm(from,to,1.0,_)
      }.backtrack |
    (realnP ~ (sps *> char(',') *> sps *> realnP) ~
      (sps *> char(',') *> sps *> string("...") *> sps *>
      (char(',') *> sps *> realnP).?) <* char(']'))
      .map{
        case ((from1,from2),to) => SeqStrm(from1,to,from2-from1,_)
      }

    // (realnP ~ (sps *> char(',') *> sps *> string("...") *> sps *> char(',') *>
    //   sps *> realnP).?).map{
    //     case (from,Some(to)) => SeqStrm(from,to,1.0,false)
    //     case (from,None) => SeqStrm(from,Double.PositiveInfinity,1.0,false)
    //   } |
    // (char('[') *> realnP.repSep(sps *> char(',') *> sps) <* char(']'))
    //   .map(x => ListStrm(x.toList,false))

  def assign: P[String => Program] =
    (string(":=") *> sps *> expr <* sps <* char(';')).map(e => v => Assign(v,e))

  def diffEq: P[String => Program] =
    ((char('\'') *> sps *> char('=') *> sps *> expr <* sps) ~ // 1st expr
      ((char(',')*>sps*>varName) ~ (char('\'') *> sps *> char('=') *> sps *> expr <* sps)).rep0 ~ // (x2'=e2)*
      duration)//(string("for") *> sps *> expr <* (sps <* char(';')))) // for dur;
      .map{
        case ((e1,x2e2s),appDur) => x1 => appDur(Map(x1->e1)++x2e2s.toMap)
      }
  // "for" or "until" (syntactic sugar)
  def duration: P[Map[String,Expr] => Program] =
    string("forever") *> sps *> char(';').as(eqs => EqDiff(eqs,None)) |
    string("for") *> sps *>
      expr.map(dur => eqs => EqDiff(eqs,Some(dur))) <*
      (sps <* char(';')) |
    ((string("until_") *> expr) ~ (sps *> cond <* (sps <* char(';'))))
      .map((dur,c) => (eqs:Map[String,Expr]) => While(Expr.Func("!",List(c)), EqDiff(eqs, Some(dur))))


  def suffix: P[String => Program] =
    string("++") *> sps *> char(';')
      .as(v => Assign(v,Expr.Func("+",List(Expr.Var(v),Expr.Num(1))))) |
    string("--") *> sps *> char(';')
      .as(v => Assign(v, Expr.Func("-", List(Expr.Var(v), Expr.Num(1)))))

  def expr: P[Expr] = P.recursive((recExpr: P[Expr]) => {
    def literal: P[Expr] = P.recursive((recLit: P[Expr]) =>
      (char('(') *> recExpr.surroundedBy(sps) <* char(')')) |
      (char('-') ~ recLit).map(x => Expr.Func("*",List(Expr.Num(-1),x._2))) |
      (char('!') ~ recLit).map(x => Expr.Func("!",List(x._2))) | // cond NOT
      realnP.map(Expr.Num.apply) |
      string("true").as(Expr.True) |
      string("false").as(Expr.False) |
      (varName~(sps *> (char('(') *> sps *> recExpr.repSep0(sps~char(',')~sps) <* (sps <* char(')'))).?))
        .map {
          case (v, None) => Expr.Var(v)
          case (v, Some(args)) => preProcess(Expr.Func(v, args))
      })

    def pow: P[(Expr, Expr) => Expr] =
      string("^").as((x: Expr, y: Expr) => Expr.Func("^",List(x,y)))

    def mult: P[(Expr, Expr) => Expr] =
      string("*").as((x: Expr, y: Expr) => Expr.Func("*",List(x,y))) |
      string("/").as((x: Expr, y: Expr) => Expr.Func("/",List(x,y)))

    def sum: P[(Expr, Expr) => Expr] =
      string("+").as((x: Expr, y: Expr) => Expr.Func("+",List(x,y))) |
      string("-").as((x: Expr, y: Expr) => Expr.Func("-",List(x,y)))

    def comp: P[(Expr, Expr) => Expr] =
      (string("<=")| string(">=")| char('<')| char('>')| string("==")| string("!="))
        .string
        .map(op => ((e1,e2) => Expr.Func(op,List(e1,e2))))

    def or: P[(Expr, Expr) => Expr] =
      (string("||")|string("\\/")).as((x,y) => Expr.Func("||",List(x,y)))

    def and: P[(Expr, Expr) => Expr] =
      (string("&&")|string("/\\")).as((x,y) => Expr.Func("&&",List(x,y)))

    listSep(listSep(listSep(listSep(listSep(listSep(
      literal, pow), mult), sum), comp), and), or)
  })

  /** Replaces some functions with its pre-processed equivalent. */
  def preProcess(f:Expr.Func): Expr = f match {
    case Expr.Func("unif",List(e1,e2)) =>
      // unif*(e2-e1)+e1
      Expr.Func("+",List(e1,
        Expr.Func("*",List(Expr.Func("unif",Nil),
          Expr.Func("-",List(e2,e1))
        ))
      )) 
    case Expr.Func("expn",List(lamb)) => // - ln ( unif ) / lambda
      Expr.Func("/",List(Expr.Func("*",List(Expr.Num(-1),
        Expr.Func("ln",List(Expr.Func("unif",Nil))))),lamb))
    /* x1 := unif (0 ,1) ;
       x2 := unif (0 ,1) ;
       x := sqrt ( -2 * ( ln x1 ) ) * cos (2 * pi * x2 )
       x := m + s * x
     */
    case Expr.Func("normal",List(m,s)) => // - ln ( unif ) / lambda
      Expr.Func("+", List(
        m,
        Expr.Func("*",List(
          s,
          Expr.Func("*",List(
            Expr.Func("sqrt",List(
              Expr.Func("*",List(
                Expr.Num(-2.0),
                Expr.Func("ln",List(
                  Expr.Func("unif",Nil)
                ))
              ))
            )),
            Expr.Func("cos",List(
              Expr.Func("*",List(
                Expr.Func("*",List(
                  Expr.Num(2.0),
                  Expr.Func("pi",Nil)
                )),
                Expr.Func("unif",Nil)
              ))
            ))
          ))
        ))
      ))
           // xmin ⋅ (1−unif)^{−1/(alpha−1)}, where alpha>1 and xmin>0
     // (here using unif instead of 1-unif)
     case Expr.Func("powerlaw",List(alpha,xmin)) =>
      Expr.Func("*",List(
        xmin,
        Expr.Func("pow",List(
          Expr.Func("unif",Nil),
          Expr.Func("/",List(
            Expr.Num(-1),
            Expr.Func("-",List(
              alpha,
              Expr.Num(1)
            ))
          ))
        ))
      ))
    case _ => f
  }


  /** Parse a boolean condition */
  def cond: P[Expr] = expr




  def plotInfo: P[PlotInfo] =
    (char('-').rep *> sps *> (plotMod<*sps).rep).map(lst =>
      lst.foldLeft(PlotInfo.default)((p,f) => f(p))
    )

  def plotMod: P[PlotInfo => PlotInfo] =
    plotModBuild("until", realP, pi => r => 
        pi.copy(maxTime = r)) |
    plotModBuild("from" , realP, pi => r =>
        pi.copy(minTime = r)) |
    plotModBuild("iterations", intP, pi => r =>
        pi.copy(maxLoops = r)) |
    plotModBuild("samples", intP, pi => r =>
        pi.copy(samples = r)) |
    plotModBuild("rk-samples", intP, pi => r =>
        pi.copy(rkSamples = r)) |
    plotModBuild("seed", intP, pi => r =>
        pi.copy(seed = r)) |
    plotModBuild("vars", regExp, pi => r =>
        pi.copy(showVar = str => r.exists(re => re.r.matches(str)))) |
    plotModBuild("height", intP, pi => r =>
        pi.copy(height = r)) |
    plotModBuild("runs", intP, pi => r =>
        pi.copy(runs = r)) |
    plotModBuild("monitor-sample-freq", realP, pi => r =>
        pi.copy(monSampleFreq = r)) |
    plotModBuild("monitor-sample-noise", realP, pi => r =>
        pi.copy(monSampleNoise = r)) |
    plotModBuild("portrait", portraitArgs, pi => lst =>
        pi.copy(portrait = lst ::: pi.portrait)) |
    string("verbose").map(r =>
      (pi: PlotInfo) => pi.copy(showAll = true))

  def portraitArgs: P[List[(String,String)]] =
      (varName ~ (sps *> char(',') *> sps *>(varName <* sps))).repSep(char(';')*>sps)
        .map(lst => lst.toList)

  def plotModBuild[A](kw:String, args:P[A], upd:PlotInfo => A => PlotInfo): P[PlotInfo => PlotInfo] =
    string(kw) *> sps *> args.map((as:A) => (pi: PlotInfo) => upd(pi)(as))

  //// Auxiliary functions

  def listSep[A](elem: P[A], op: P[(A, A) => A]): P[A] =
    (elem ~ (op.surroundedBy(sps).backtrack ~ elem).rep0)
      .map(x => {
        val pairlist = x._2
        val first = x._1;
        pairlist.foldLeft(first)((rest, pair) => pair._1(rest, pair._2))
      })
