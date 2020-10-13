package cs320

import scala.util.parsing.combinator._
import scala.collection.immutable.{Set => ISet}

sealed trait Expr

case class Num(num: Int) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Id(name: String) extends Expr
case class Fun(param: String, body: Expr) extends Expr
case class App(fun: Expr, arg: Expr) extends Expr
case class NewBox(expr: Expr) extends Expr
case class SetBox(box: Expr, expr: Expr) extends Expr
case class OpenBox(box: Expr) extends Expr
case class Seqn(left: Expr, right: List[Expr]) extends Expr
case class Rec(fields: List[(String, Expr)]) extends Expr
case class Get(record: Expr, field: String) extends Expr
case class Set(record: Expr, field: String, expr: Expr) extends Expr

object Expr extends RegexParsers {

  def wrapC[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
  def wrapR[T](rule: Parser[T]): Parser[T] = "(" ~> rule <~ ")"

  lazy val int: Parser[Int] = """-?\d+""".r ^^ BigInt.apply

  val keywords = ISet("val", "Box", "set", "get")

  lazy val str: Parser[String] =
    "[a-zA-Z_][a-zA-Z0-9_]*".r.withFilter(!keywords(_))

  lazy val expr: Parser[Expr] =
    e1 ~ rep(
      wrapR(expr) ^^ EApp |
      "." ~> "set" ~> wrapR(expr) ^^ ESetBox |
      "." ~ "get" ^^^ EOpenBox |
      "." ~> str ^^ EGet
    ) ^^ { case e ~ es => es.foldLeft(e){
      case (e, EApp(a)) => App(e, a)
      case (e, ESetBox(a)) => SetBox(e, a)
      case (e, EOpenBox) => OpenBox(e)
      case (e, EGet(f)) => Get(e, f)
    }}
  lazy val e1: Parser[Expr] =
    "Box" ~> wrapR(expr) ^^ NewBox | int ^^ Num | str ^^ Id |
    wrapR((expr <~ "+") ~ expr) ^^ { case l ~ r => Add(l, r) } |
    wrapR((expr <~ "-") ~ expr) ^^ { case l ~ r => Sub(l, r) } |
    wrapC(str ~ ("=>" ~> expr)) ^^ { case p ~ b => Fun(p, b) } |
    wrapC(rep1sep(expr, ";")) ^^ {
      case Nil => error("Seqn cannot be empty")
      case l :: r => Seqn(l, r)
    } |
    wrapC(repsep(str ~ ("=" ~> expr), ",")) ^^ (fs => {
      val l = fs.map{ case f ~ e => (f, e) }
      val ns = l.map(_._1)
      if (dupCheck(ns)) error(s"duplicate fields: $ns")
      Rec(l)
    }) |
    wrapC(rExpr ~ ("=" ~> expr)) ^^ { case (r, f) ~ e => Set(r, f, e) }
  lazy val rExpr: Parser[(Expr, String)] =
    expr ^? { case Get(e, f) => (e, f) }

  sealed trait E
  case class EApp(a: Expr) extends E
  case class ESetBox(e: Expr) extends E
  case object EOpenBox extends E
  case class EGet(f: String) extends E

  def dupCheck(ss: List[String]): Boolean = ss.distinct.length != ss.length

  def apply(str: String): Expr = parseAll(expr, str).get
}
