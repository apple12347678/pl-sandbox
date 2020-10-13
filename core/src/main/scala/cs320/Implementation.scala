package cs320

import Value._

object Implementation extends Template {

  type Store = Map[Addr, Value]

  def malloc(store: Store): Addr = store.foldLeft[Addr](0)((m, addr) => addr match {
    case (a, _) => if (m > a) m else a
  }) + 1

  def numOp(f: (Int, Int) => Int): (Value, Value) => NumV = (l, r) => (l, r) match {
    case (NumV(ln), NumV(rn)) => NumV(f(ln, rn))
    case _ => error()
  }

  def interpEnvStore(expr: Expr, env: Env, store: Store): (Value, Store) = expr match {
    case Num(n) => (NumV(n), store)
    case Add(l, r) => {
      val (lv, s1) = interpEnvStore(l, env, store)
      val (rv, s2) = interpEnvStore(r, env, s1)
      (numOp(_ + _)(lv, rv), s2)
    }
    case Sub(l, r) => {
      val (lv, s1) = interpEnvStore(l, env, store)
      val (rv, s2) = interpEnvStore(r, env, s1)
      (numOp(_ - _)(lv, rv), s2)
    }
    case Id(n) => (env.getOrElse(n, error()), store)
    case Fun(p, b) => (CloV(p, b, env), store)
    case App(f, a) => {
      val (fun, s1) = interpEnvStore(f, env, store)
      fun match {
        case CloV(p, b, fe) => {
          val (param, s2) = interpEnvStore(a, env, s1)
          interpEnvStore(b, fe + (p -> param), s2)
        }
        case _ => error()
      }
    }
    case NewBox(e) => {
      val (bv, s1) = interpEnvStore(e, env, store)
      bv match {
        case NumV(n) => (BoxV(n), s1 + (n -> bv))
        case _ => error()
      }
    }
    case SetBox(b, e) => {
      val (bv, s1) = interpEnvStore(b, env, store)
      bv match {
        case BoxV(addr) => {
          val (sv, s2) = interpEnvStore(e, env, s1)
          (sv, s2 + (addr -> sv))
        }
        case _ => error()
      }
    }
    case OpenBox(b) => {
      val (bv, s1) = interpEnvStore(b, env, store)
      bv match {
        case BoxV(addr) => {
          (s1.getOrElse(addr, error()), s1)
        }
        case _ => error()
      }
    }
    case Seqn(l, r) => {
      val (lv, ls) = interpEnvStore(l, env, store)
      r match {
        case Nil => (lv, ls)
        case rle :: rre => interpEnvStore(Seqn(rle, rre), env, ls)
      }
    }
    case Rec(f) => {
      val recRes = f.foldLeft((Map[String, Addr](), store))(
        (acc, v) => acc match {
          case (rec: Map[String, Addr], sto: Store) => {
            val (tv, ts) = interpEnvStore(v._2, env, sto)
            val addr = malloc(ts)
            (rec + (v._1 -> addr), ts + (addr -> tv))
          }
        }
      )
      (RecV(recRes._1), recRes._2)
    }
    case Get(r, id) => interpEnvStore(r, env, store) match {
      case (RecV(f), s1: Store) => {
        val addr = f.getOrElse(id, error("no such field"))
        (s1.getOrElse(addr, error()), s1)
      }
      case _ => error()
    }
    case Set(r, id, e) => interpEnvStore(r, env, store) match {
      case (RecV(f), s1: Store) => {
        val (tv, ts)= interpEnvStore(e, env, s1)
        val addr = f.getOrElse(id, error())
        (tv, ts + (addr -> tv))
      }
      case _ => error()
    }
  }

  def interp(expr: Expr): Value = interpEnvStore(expr, Map(), Map())._1

}
