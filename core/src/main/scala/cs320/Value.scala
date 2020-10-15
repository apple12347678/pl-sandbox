package cs320

sealed trait Value

object Value {

  type Env = Map[String, Addr]
  type Addr = Int
  type Store = Map[Addr, Value]

  case class NumV(n: Int) extends Value
  case class BoolV(value: Boolean) extends Value
  case class CloV(param: String, body: Expr, env: Env) extends Value

  def show(value: Value): String = value match {
    case NumV(n) => n.toString
    case BoolV(v) => v.toString
    case CloV(p, b, e) => s"<lambda ${p}>"
  }

}
