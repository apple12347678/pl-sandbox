package cs320

trait Template {

  def run(str: String): String = Value.show(interpStrict(Expr(str)))

  def interpMain(expr: Expr): Value

  def interpStrict(expr: Expr): Value

}
