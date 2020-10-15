package cs320

import Value._

object Implementation extends Template {

  def strict(value: Value): Value = value match {
    case ev@ExprV(expr, env, cache) => cache match {
      case Some(v) => v
      case None => {
        val v = interp(expr, env)
        ev.cache = Some(v)
        v
      }
    }
    case e => e
  }

  def numOp(f: (Int, Int) => Int): (Value, Value) => NumV = (l, r) => (strict(l), strict(r)) match {
    case (NumV(ln), NumV(rn)) => NumV(f(ln, rn))
    case _ => error()
  }

  def boolOp(f: (Boolean, Boolean) => Boolean): (Value, Value) => BoolV = (l, r) => (strict(l), strict(r)) match {
    case (BoolV(lb), BoolV(rb)) => BoolV(f(lb, rb))
    case _ => error()
  }

  def numToBoolOp(f: (Int, Int) => Boolean): (Value, Value) => BoolV = (l, r) => (strict(l), strict(r)) match {
    case (NumV(lb), NumV(rb)) => BoolV(f(lb, rb))
    case _ => error()
  }

  def interp(expr: Expr, env: Env): Value = expr match {
    case NumE(n) => NumV(n)
    case BoolE(b) => BoolV(b)
    case Add(l, r) => numOp(_ + _)(interp(l, env), interp(r, env))
    case Sub(l, r) => numOp(_ - _)(interp(l, env), interp(r, env))
    case If(condition, tBranch, fBranch) => interp(condition, env) match {
      case BoolV(b) => if (b) interp(tBranch, env) else interp(fBranch, env)
      case _ => error()
    }
    case Eq(l, r) => numToBoolOp(_ == _)(interp(l, env), interp(r, env))
    case Lt(l, r) => numToBoolOp(_ < _)(interp(l, env), interp(r, env))
    case Id(name) => env.getOrElse(name, error())
    case Fun(param, body) => CloV(param, body, env)
    case App(func, arg) => interp(func, env) match {
      case CloV(param, body, fenv) => interp(body, fenv + (param -> ExprV(arg, env, None)))
      case _ => error()
    }
    case _ => error()
  }

  def interpMain(expr: Expr): Value = strict(interp(expr, Map()))

}
