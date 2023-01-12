enum Expr:
  case Binary(left: Expr, operator: Token, right: Expr)
  case Grouping(expression: Expr)
  case Literal(value: Object)
  case Unary(operator: Token, right: Expr)

