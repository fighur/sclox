enum Stmt:
  case Class(
      name: Token,
      superclass: Option[Expr.Variable],
      methods: List[Stmt.Function]
  )
  case Expression(expr: Expr)
  case Print(expr: Expr)
  case Return(keyword: Token, value: Option[Expr])
  case Var(name: Token, initializer: Option[Expr])
  case Function(name: Token, params: List[Token], body: List[Stmt])
  case Block(statements: List[Stmt])
  case If(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt])
  case While(condition: Expr, body: Stmt)
