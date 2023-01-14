enum Expr:
  case Binary(left: Expr, operator: Token, right: Expr)
  case Call(callee: Expr, paren: Token, arguments: List[Expr])
  case Grouping(expression: Expr)
  case Literal(value: Any)
  case Unary(operator: Token, right: Expr)
  case Variable(name: Token)
  case Assign(name: Token, value: Expr)
  case Logical(left: Expr, operator: Token, right: Expr)