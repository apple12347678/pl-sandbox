package cs320

import Macros._

class Spec extends SpecBase {

  val run = Implementation.run _

  // test exprs
  // test(run(""), "")

  test(run("42"), "42")
  test(run("1 + 2"), "3")
  test(run("7 - 2"), "5")
  test(run("1 - -1"), "2")
  test(run("true"), "true")
  test(run("1 == 3 - 2"), "true")
  test(run("1 < 3 - 2"), "false")
  test(run("x => x + x"), "<lambda x>")
  test(run("{x => x + x}(1)"), "2")
  test(run("{x => y => x + y}(1)(2)"), "3")
  test(run("if (true) 1 else 2"), "1")
  test(run("!true"), "false")
  test(run("true && false"), "false")
  test(run("true || false"), "true")
  test(run("1 != 2"), "true")
  test(run("1 > 1"), "false")
  test(run("1 >= 1"), "true")

  /* Write your own tests */

  /* free identifier */
  testExc(run("""
    val x = 1 + 3;
    z
  """), "")

  /* shadowing */
  test(run("""
    val x = 4;
    ((
      val x = 5;
      x + 3
    ) + x)
  """), "12")

  /* type errors */
  testExc(run("1 + true"), "")
  testExc(run("1 && true"), "")

  //Integer & Boolean Expression
  test(run("45"), "45")
  test(run("100"), "100")
  test(run("348579"), "348579")
  test(run("6432"), "6432")

  test(run("false"), "false")

  //Integer Operation
  /* -n */
  test(run("-4"), "-4")
  test(run("-67"), "-67")
  test(run("""
    val a = 5;
    -a
  """), "-5")
  testExc(run("-false"), "")

  /* comparison */
  test(run("1 < 2"), "true")
  test(run("1 < 1"), "false")
  test(run("1 <= 1"), "true")
  test(run("1 <= 0"), "false")
  test(run("2 > 1"), "true")
  test(run("2 > 2"), "false")
  test(run("8 >= 7"), "true")
  test(run("6 >= 7"), "false")
  test(run("5 == 5"), "true")
  test(run("7 == 6"), "false")
  test(run("6 != 7"), "true")
  test(run("8 != 8"), "false")
  testExc(run("true == 78"), "")

  //Conditional Branch
  test(run("if (true) true else false"), "true")
  test(run("if (1 > 2) 4 else 7"), "7")
  test(run("!false"), "true")
  test(run("!!false"), "false")
  test(run("false && true"), "false")
  test(run("false && false"), "false")
  test(run("false || true"), "true")
  test(run("false || false"), "false")
  testExc(run("if (6) 4 else 5"), "")

  //Variable
  test(run("""
    val y = 6;
    y + 2
  """), "8")
  test(run("""
    val k = true;
    k
  """), "true")
  testExc(run("k"), "")

  //Function & Application
  test(run("{x => x + 6}"), "<lambda x>")
  test(run("{y => y + 6}(5)"), "11")
  test(run("{x => y => z => x + y + z}(1)(2)(3)"), "6")
  test(run("{(x, w) => w - x + 2}(2, 3)"), "1")
  test(run("{x => y => z => x - y + z}(1)"), "<lambda y>")
  testExc(run("false(2)"), "")

}
