import Expr.*

class AstPrinter:
  def print(expr: Expr): String = expr match
    case Binary(left, operator, right) =>
      parenthesize(operator.lexeme, left, right)
    case Grouping(expression) => parenthesize("group", expression)
    case Literal(value)       => if value == null then "nil" else value.toString
    case Unary(operator, right) => parenthesize(operator.lexeme, right)
    case _                      => ???

  private def parenthesize(name: String, exprs: Expr*): String =
    exprs.foldLeft(s"($name")((str, expr) => str + s" ${print(expr)}") + ")"

@main def testPrinter: Unit =
  import TokenType.*

  // (5 - (3 - 1)) + -1
  val expression: Expr = Expr.Binary(
    Expr.Grouping(
      Expr.Binary(
        Expr.Literal(5),
        Token(MINUS, "-", null, 1),
        Expr.Grouping(
          Expr.Binary(
            Expr.Literal(3),
            Token(MINUS, "-", null, 1),
            Expr.Literal(1)
          )
        )
      )
    ),
    Token(PLUS, "+", null, 1),
    Expr.Unary(Token(MINUS, "-", null, 1), Expr.Literal(1))
  )

  println(AstPrinter().print(expression))
