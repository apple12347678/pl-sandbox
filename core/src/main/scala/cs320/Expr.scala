package cs320

import scala.util.parsing.combinator._
import scala.collection.immutable._

sealed trait Expr

case class NumE(num: Int) extends Expr
case class BoolE(value: Boolean) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Eq(left: Expr, right: Expr) extends Expr
case class Lt(left: Expr, right: Expr) extends Expr
case class If(condition: Expr, trueBranch: Expr, falseBranch: Expr) extends Expr
case class Id(name: String) extends Expr
case class Fun(param: String, body: Expr) extends Expr
case class App(fun: Expr, arg: Expr) extends Expr

object Expr extends RegexParsers {

  def wrapC[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
  def wrapR[T](rule: Parser[T]): Parser[T] = "(" ~> rule <~ ")"
  def wrapT[T](rule: Parser[T]): Parser[T] = "[" ~> rule <~ "]"

  private lazy val keywords = Set("val", "if", "else", "val", "true", "false")

  private lazy val n: Parser[Int] = "-?[0-9]+".r ^^ BigInt.apply

  private lazy val b: Parser[Boolean] = "true" ^^^ true | "false" ^^^ false

  private lazy val x: Parser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r.withFilter(!keywords(_))

  private lazy val expr: Parser[Expr] =
    e ~ rep(wrapR(repsep(expr, ",")) ^^ EApp) ^^ {
      case e ~ es => es.foldLeft(e){
        case (e, EApp(as)) => MApp(e, as)
      }
    }

  private lazy val e: Parser[Expr] =
    (x <~ "=>") ~ e ^^ { case p ~ b => Fun(p, b) } |
    (wrapR(repsep(x, ",")) <~ "=>") ~ e ^^ {
      case ps ~ b =>
        if (dupCheck(ps))
          error(s"Duplicated parameters: ${ps.mkString(", ")}")
        MFun(ps, b)
    } |
    wrapT(rep1sep(expr, ";")) ^^ {
      case Nil => error("Seqn cannot be empty")
      case l :: r => Seqn(l, r)
    } | e1

  private lazy val e1: Parser[Expr] =
    rep1sep(e2, "||") ^^ (_.reduceLeft(Or)) | wrapR(e1)

  private lazy val e2: Parser[Expr] =
    rep1sep(e3, "&&") ^^ (_.reduceLeft(And)) | wrapR(e2)

  private lazy val e3: Parser[Expr] =
    e4 ~ rep(("==" | "!=" | "<=" | "<" | ">=" | ">") ~ e4) ^^ {
      case e ~ es => es.foldLeft(e){
        case (l, "==" ~ r) => Eq(l, r)
        case (l, "!=" ~ r) => Neq(l, r)
        case (l, "<"  ~ r) => Lt(l, r)
        case (l, "<=" ~ r) => Lte(l, r)
        case (l, ">"  ~ r) => Gt(l, r)
        case (l,   _  ~ r) => Gte(l, r)
      }
    } | wrapR(e3)

  private lazy val e4: Parser[Expr] =
    e5 ~ rep(("+" | "-") ~ e5) ^^ { case e ~ es => es.foldLeft(e){
      case (l, "+" ~ r) => Add(l, r)
      case (l,  _  ~ r) => Sub(l, r)
    }} | wrapR(e4)

  private lazy val e5: Parser[Expr] = "-" ~> e5 ^^ Neg | "!" ~> e5 ^^ Not | e6

  private lazy val e6: Parser[Expr] =
    x ^^ Id | n ^^ NumE | b ^^ BoolE |
    ("if" ~> wrapR(e)) ~ e ~ ("else" ~> e) ^^ { case c ~ t ~ f => If(c, t, f) } |
    ("val" ~> x <~ "=") ~ e ~ (";" ~> e) ^^ { case x ~ e ~ b => Val(x, e, b) } |
    wrapC(e)

  private sealed trait E
  private case class EApp(as: List[Expr]) extends E

  // Desugaring
  private val T = BoolE(true)
  private val F = BoolE(false)
  private def Val(n: String, e: Expr, b: Expr) = App(Fun(n, b), e)
  private def Not(e: Expr): Expr = If(e, F, T)
  private def Neg(n: Expr): Expr = Sub(NumE(0), n)
  private def Neq(l: Expr, r: Expr): Expr = Not(Eq(l, r))
  private def Lte(l: Expr, r: Expr): Expr = {
    val lv, rv = fresh()

    Val(lv, l,
    Val(rv, r,
    Or(Eq(Id(lv), Id(rv)), Lt(Id(lv), Id(rv)))))
  }
  private def Gt(l: Expr, r: Expr): Expr = Not(Lte(l, r))
  private def Gte(l: Expr, r: Expr): Expr = Not(Lt(l, r))
  private def And(l: Expr, r: Expr): Expr = If(l, r, F)
  private def Or(l: Expr, r: Expr): Expr = If(l, T, r)
  private def Seqn(l: Expr, r: List[Expr]): Expr = r match {
    case Nil => l
    case rv :: seq => {
      val param = fresh()
      App(Fun(param, Seqn(rv, seq)), l)
    }
  }
  private def MFun(params: List[String], body: Expr): Expr = {
    params.foldLeft(body){
      case (acc: Expr, param: String) => Fun(param, acc)
    }
  }
  private def MApp(expr: Expr, args: List[Expr]): Expr = {
    args.foldLeft(expr){
      case (acc: Expr, arg: Expr) => App(acc, arg)
    }
  }

  private var id = -1
  private def fresh(): String = {
    id += 1
    s"$$x$id"
  }

  def dupCheck(ss: List[String]): Boolean = ss.distinct.length != ss.length

  def apply(str: String): Expr = parseAll(expr, str).get
}
