import Expr.*

class AstPrinter:
  def print(expr: Expr): String = expr match
    case Binary(left, operator, right) => parenthesize(operator.lexeme, left, right)
    case Grouping(expression) => parenthesize("group", expression)
    case Literal(value) => if value == null then "nil" else value.toString
    case Unary(operator, right) => parenthesize(operator.lexeme, right)
    case _ => ???


  private def parenthesize(name: String, exprs: Expr*): String =
    exprs.foldLeft(s"($name")((str, expr) => str + s" ${print(expr)})")


@main def testPrinter: Unit =
  val expression: Expr = Expr.Binary(
    Expr.Unary(
      Token(TokenType.MINUS, "-", null, 1),
      Expr.Literal(123.asInstanceOf[Object])),
    Token(TokenType.STAR, "*", null, 1),
    Expr.Grouping(
      Expr.Literal(45.67.asInstanceOf[Object])))

  println(AstPrinter().print(expression))