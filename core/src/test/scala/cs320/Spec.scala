package cs320

import Macros._

class Spec extends SpecBase {

  val run = Implementation.run _

  // test exprs
  // test(run(""), "")
  test(run("1"), "1")
  test(run("true"), "true")
  test(run("{ x => x }"), "<lambda x>")
  
  test(run("(1 + 2)"), "3")
  test(run("(3 - 2)"), "1")

  test(run("true && false"), "false")
  test(run("false || true"), "true")

  test(run("(1 == 2)"), "false")
  test(run("(3 == 3)"), "true")
  test(run("(3 >= 3)"), "true")
  test(run("(3 <= 3)"), "true")
  test(run("(3 > 3)"), "false")
  test(run("(3 < 3)"), "false")

}
