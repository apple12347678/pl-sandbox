package cs320

import Macros._

class Spec extends SpecBase {

  val run = Implementation.run _

  test(run("{ 1; 2 }"), "2")
  test(run("{ b => b.get }(Box(10))"), "10")
  test(run("{ b => { b.set(12); b.get } }(Box(10))"), "12")
  test(run("{ b => b.get }({ Box(9); Box(10) })"), "10")
  test(run("{ b => { a => b.get } }(Box(9))(Box(10))"), "9")
  test(run("{ b => { b.set(2); b.get } }(Box(1))"), "2")
  test(run("{ b => { b.set((9 + b.get)); b.get } }(Box(1))"), "10")
  test(run("{ b => { b.set((2 + b.get)); b.set((3 + b.get)); b.set((4 + b.get)); b.get } }(Box(1))"), "10")
  test(run("{ r => r.x }({ x = 1 })"), "1")
  test(run("{ r => { { r.x = 5 }; r.x } }({ x = 1 })"), "5")
  test(run("{ g => { s => { r1 => { r2 => (r1.b + { s(r1)(g(r2)); ({ s(r2)(g(r1)); r1.b } + r2.b) }) } } } }({ r => r.a })({ r => { v => { r.b = v } } })({ a = 0, b = 2 })({ a = 3, b = 4 })"), "5")
  test(run("{ x => x }"), "<function>")
  test(run("Box(1)"), "<box>")
  test(run("{}"), "<record>")
  testExc(run("{ x = 1 }.y"), "no such field")

  /* Write your own tests */
}
