package cs320

import Macros._

class Spec extends SpecBase {

  val run = Implementation.run _

  // test exprs
  test(run("1"), "1")
  test(run("true"), "true")
  test(run("{ x => x }"), "<lambda x>")

}
