package cs320

trait Template {

  def strict(value: Value): Value

  def run(str: String): String = Value.show(strict(interpMain(Expr(str))))

  def interpMain(expr: Expr): Value

}
