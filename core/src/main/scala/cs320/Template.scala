package cs320

trait Template {

  def run(str: String): String = Value.show(interpMain(Expr(str)))

  def interpMain(expr: Expr): Value

}
