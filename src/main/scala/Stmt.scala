enum Stmt:
  case Expression(expr: Expr)
  case Print(expr: Expr)
  case Var(name: Token, initializer: Expr)
  case Block(statements: List[Stmt])