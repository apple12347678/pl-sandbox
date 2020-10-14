package cs320

import scala.util.parsing.combinator._
import scala.collection.immutable._

sealed trait Expr

case class NumE(num: Int) extends Expr
case class BoolE(value: Boolean) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Id(name: String) extends Expr
// case class Val(name: String, expression: Expr, body: Expr) extends Expr
case class Fun(param: String, body: Expr) extends Expr
case class App(fun: Expr, arg: Expr) extends Expr
case class Seqn(left: Expr, right: List[Expr]) extends Expr
case class Eq(left: Expr, right: Expr) extends Expr
case class Lt(left: Expr, right: Expr) extends Expr
case class If(condition: Expr, trueBranch: Expr, falseBranch: Expr) extends Expr

object Expr extends RegexParsers {

  def wrapC[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
  def wrapR[T](rule: Parser[T]): Parser[T] = "(" ~> rule <~ ")"

  private lazy val keywords = Set("val", "if", "else", "val", "true", "false")

  private lazy val str: Parser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r.withFilter(!keywords(_))

  private lazy val n: Parser[Int] = "-?[0-9]+".r ^^ BigInt.apply

  private lazy val b: Parser[Boolean] = "true" ^^^ true | "false" ^^^ false

  private lazy val x: Parser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r.withFilter(!keywords(_))

  private lazy val expr: Parser[Expr] =
    e ~ rep(wrapR(expr) ^^ EApp) ^^ {
      case e ~ es => es.foldLeft(e){
        case (e, EApp(a)) => App(e, a)
      }
    }

  private lazy val e: Parser[Expr] =
    n ^^ NumE | str ^^ Id |
    wrapR((expr <~ "+") ~ expr) ^^ { case l ~ r => Add(l, r) } |
    wrapR((expr <~ "-") ~ expr) ^^ { case l ~ r => Sub(l, r) } |
    wrapR((expr <~ "==") ~ expr) ^^ { case l ~ r => Eq(l, r) } |
    wrapR((expr <~ "!=") ~ expr) ^^ { case l ~ r => Neq(l, r) } |
    wrapR((expr <~ "<") ~ expr) ^^ { case l ~ r => Lt(l, r) } |
    wrapR((expr <~ "<=") ~ expr) ^^ { case l ~ r => Lte(l, r) } |
    wrapR((expr <~ ">") ~ expr) ^^ { case l ~ r => Gt(l, r) } |
    wrapR((expr <~ ">=") ~ expr) ^^ { case l ~ r => Gte(l, r) } |
    wrapC(str ~ ("=>" ~> expr)) ^^ { case p ~ b => Fun(p, b) } |
    wrapC(rep1sep(expr, ";")) ^^ {
      case Nil => error("Seqn cannot be empty")
      case l :: r => Seqn(l, r)
    } | e1

  private lazy val e1: Parser[Expr] =
    rep1sep(e2, "||") ^^ (_.reduceLeft(Or))

  private lazy val e2: Parser[Expr] =
    rep1sep(e4, "&&") ^^ (_.reduceLeft(And))

  // private lazy val e3: Parser[Expr] =
  //   e4 ~ rep(("==" | "!=" | "<=" | "<" | ">=" | ">") ~ e4) ^^ {
  //     case e ~ es => es.foldLeft(e){
  //       case (l, "==" ~ r) => Eq(l, r)
  //       case (l, "!=" ~ r) => Neq(l, r)
  //       case (l, "<"  ~ r) => Lt(l, r)
  //       case (l, "<=" ~ r) => Lte(l, r)
  //       case (l, ">"  ~ r) => Gt(l, r)
  //       case (l,   _  ~ r) => Gte(l, r)
  //     }
  //   }

  private lazy val e4: Parser[Expr] = "!" ~> e4 ^^ Not | e5

  private lazy val e5: Parser[Expr] =
    x ^^ Id | n ^^ NumE | b ^^ BoolE |
    ("if" ~> wrapR(e)) ~ e ~ ("else" ~> e) ^^ { case c ~ t ~ f => If(c, t, f) } |
    ("val" ~> x <~ "=") ~ e ~ (";" ~> e) ^^ { case x ~ e ~ b => Val(x, e, b) } |
    wrapC(e)

  sealed trait E
  case class EApp(a: Expr) extends E

  // Desugaring
  private val T = BoolE(true)
  private val F = BoolE(false)
  private def Val(n: String, e: Expr, b: Expr) = App(Fun(n, b), e)
  private def Not(e: Expr): Expr = If(e, F, T)
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

  private var id = -1
  private def fresh(): String = {
    id += 1
    s"$$x$id"
  }

  def dupCheck(ss: List[String]): Boolean = ss.distinct.length != ss.length

  def apply(str: String): Expr = parseAll(expr, str).get
}
