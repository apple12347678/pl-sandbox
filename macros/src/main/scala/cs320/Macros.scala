package cs320

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

object Macros {
  def testImpl(c: Context)(f: c.Tree, v: c.Tree): c.Tree = {
    import c.universe._
    q"""{
      import scala.reflect.runtime.universe._
      val code = normalize(show(reify{ $f }.tree))
      val value = normalize(show(reify{ $v }.tree))
      code should "be " ++ value in { assert($f == $v) }
    }"""
  }

  def test(f: Any, v: Any): Unit = macro testImpl

  def testExcImpl(c: Context)(f: c.Tree, msg: c.Tree): c.Tree = {
    import c.universe._
    q"""{
      import scala.reflect.runtime.universe._
      val code = normalize(show(reify{ $f }.tree))
      val m = $msg
      code should "throw \"" ++ m + '\"' in {
        val caught = intercept[PLError] { $f }
        assert(caught.msg.contains(m))
      }
    }"""
  }

  def testExc(f: Any, msg: String): Unit = macro testExcImpl
}
