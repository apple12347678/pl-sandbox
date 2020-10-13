package cs320

trait Template {

  def run(str: String): String = Value.show(interp(Expr(str)))

  def interp(expr: Expr): Value

}
