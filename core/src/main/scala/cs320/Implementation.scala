package cs320

import Value._

object Implementation extends Template {

  type Store = Map[Addr, Value]

  def numOp(f: (Int, Int) => Int): (Value, Value) => NumV = (l, r) => (l, r) match {
    case (NumV(ln), NumV(rn)) => NumV(f(ln, rn))
    case _ => error()
  }

  def interpEnvStore(expr: Expr, env: Env, store: Store): (Value, Store) = expr match {
    case NumE(n) => (NumV(n), store)
    case BoolE(b) => (BoolV(b), store)
    case Fun(p, b) => (CloV(p, b, env), store)
    case _ => (NumV(1), store)
  }

  def interp(expr: Expr): Value = interpEnvStore(expr, Map(), Map())._1

}
