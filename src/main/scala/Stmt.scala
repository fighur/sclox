enum Stmt:
  case Expression(expr: Expr)
  case Print(expr: Expr)
  case Var(name: Token, initializer: Option[Expr])
  case Block(statements: List[Stmt])
  case If(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt])
  case While(condition: Expr, body: Stmt)