package cs320

import Value._

object Implementation extends Template {

  def numOp(f: (Int, Int) => Int): (Value, Value) => NumV = (l, r) => (l, r) match {
    case (NumV(ln), NumV(rn)) => NumV(f(ln, rn))
    case _ => error()
  }

  def boolOp(f: (Boolean, Boolean) => Boolean): (Value, Value) => BoolV = (l, r) => (l, r) match {
    case (BoolV(lb), BoolV(rb)) => BoolV(f(lb, rb))
    case _ => error()
  }

  def numToBoolOp(f: (Int, Int) => Boolean): (Value, Value) => BoolV = (l, r) => (l, r) match {
    case (NumV(lb), NumV(rb)) => BoolV(f(lb, rb))
    case _ => error()
  }

  def malloc(store: Store) = store.foldLeft[Addr](0){
    case (acc: Addr, (addr: Addr, _)) => if (acc > addr) acc else addr
  } + 1

  def interp(expr: Expr, env: Env, store: Store): (Value, Store) = expr match {
    case NumE(n) => (NumV(n), store)
    case BoolE(b) => (BoolV(b), store)
    case Add(l, r) => {
      val (lv, m1) = interp(l, env, store)
      val (rv, m2) = interp(r, env, m1)
      (numOp(_ + _)(lv, rv), m2)
    }
    case Sub(l, r) => {
      val (lv, m1) = interp(l, env, store)
      val (rv, m2) = interp(r, env, m1)
      (numOp(_ - _)(lv, rv), m2)
    }
    case Eq(l, r) => {
      val (lv, m1) = interp(l, env, store)
      val (rv, m2) = interp(r, env, m1)
      (numToBoolOp(_ == _)(lv, rv), m2)
    }
    case Lt(l, r) => {
      val (lv, m1) = interp(l, env, store)
      val (rv, m2) = interp(r, env, m1)
      (numToBoolOp(_ < _)(lv, rv), m2)
    }
    case If(condition, tBranch, fBranch) => {
      val (condv, m1) = interp(condition, env, store)
      condv match {
        case BoolV(b) => if (b) interp(tBranch, env, m1) else interp(fBranch, env, m1)
        case _ => error()
      }
    }
    case Set(name, expr) => {
      val addr = env.getOrElse(name, error())
      val (v, m1) = interp(expr, env, store)
      (v, m1 + (addr -> v))
    }
    case Id(name) => (store.getOrElse(env.getOrElse(name, error()), error()), store)
    case Fun(param, body) => (CloV(param, body, env), store)
    case App(func, arg) => interp(func, env, store) match {
      case (CloV(param, body, fenv), m1) => {
        val (pv, m2) = interp(arg, env, m1)
        val addr = malloc(m2)
        interp(body, fenv + (param -> addr), m2 + (addr -> pv))
      }
      case _ => error()
    }
    // case _ => (NumV(1), store)
  }

  def interpMain(expr: Expr): Value = interp(expr, Map(), Map())._1

}
