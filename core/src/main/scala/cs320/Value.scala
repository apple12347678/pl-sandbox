package cs320

sealed trait Value

object Value {

  type Env = Map[String, Value]
  type Addr = Int

  case class NumV(n: Int) extends Value
  case class CloV(param: String, body: Expr, env: Env) extends Value
  case class BoxV(addr: Addr) extends Value
  case class RecV(fields: Map[String, Addr]) extends Value

  def show(value: Value): String = value match {
    case NumV(n) => n.toString
    case _: CloV => "<function>"
    case _: BoxV => "<box>"
    case _: RecV => "<record>"
  }

}
