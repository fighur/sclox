import scala.collection.mutable

class Resolver(interpreter: Interpreter):
  private val scopes = mutable.Stack.empty[Map[String, Boolean]]
  private var currentFunction = FunctionType.NONE

  def resolve(statements: List[Stmt]): Unit =
    for statement <- statements do resolve(statement)


  def resolve(stmt: Stmt): Unit = stmt match
    case Stmt.Expression(expr) => resolve(expr)
    case Stmt.If(condition, thenBranch, elseBranch) =>
      resolve(condition)
      resolve(thenBranch)
      elseBranch match
        case Some(value) => resolve(value)
        case None => ()

    case Stmt.Print(expr) => resolve(expr)
    case Stmt.Return(keyword, value) =>
      if currentFunction == FunctionType.NONE then
        Lox.error(keyword, "Can't return from top-level code.")
      value match
        case Some(v) => resolve(v)
        case None => ()

    case Stmt.While(condition, body) =>
      resolve(condition)
      resolve(body)

    case Stmt.Block(statements) =>
      beginScope()
      resolve(statements)
      endScope()

    case Stmt.Var(name, initializer) =>
      declare(name)
      initializer match
        case Some(expr) => resolve(expr)
        case None => ()
      define(name)

    case function @ Stmt.Function(name, params, body) =>
      declare(name)
      define(name)
      resolveFunction(function, FunctionType.FUNCTION)

  end resolve


  def resolve(expr: Expr): Unit = expr match
    case Expr.Unary(_, right) => resolve(right)
    case Expr.Binary(left, _, right) =>
      resolve(left)
      resolve(right)

    case Expr.Call(callee, _, arguments) =>
      resolve(callee)
      for argument <- arguments do resolve(argument)

    case Expr.Grouping(expr) => resolve(expr)
    case Expr.Literal(expr) => ()
    case Expr.Logical(left, _, right) =>
      resolve(left)
      resolve(right)

    case Expr.Variable(name) =>
      if !scopes.isEmpty then
        scopes.top.get(name.lexeme) match
          case Some(false) =>
            Lox.error(name, "Can't read local variable in its own initializer.")
          case _ => ()
      resolveLocal(expr, name)

    case Expr.Assign(name, value) =>
      resolve(value)
      resolveLocal(expr, name)

  end resolve


  def resolveFunction(function: Stmt.Function, functionType: FunctionType): Unit =
    val enclosingFunction = currentFunction
    currentFunction = functionType

    beginScope()
    for param <- function.params do
      declare(param)
      define(param)
    resolve(function.body)
    endScope()

    currentFunction = enclosingFunction


  def resolveLocal(expr: Expr, name: Token): Unit =
    scopes.zipWithIndex.foldLeft(false)((resolved, scopei) =>
      if resolved then true
      else scopei match
        case (scope, i) if scope.contains(name.lexeme) =>
          interpreter.resolve(expr, i)
          true
        case _ => false)


  def beginScope(): Unit = scopes.push(Map.empty)

  def endScope(): Unit = scopes.pop()


  def declare(name: Token): Unit =
    if scopes.isEmpty then ()
    else
      val scope = scopes.pop()
      if scope.contains(name.lexeme) then
        Lox.error(name, "Already a variable with this name in this scope.")
      scopes.push(scope + (name.lexeme -> false))


  def define(name: Token): Unit =
    if scopes.isEmpty then ()
    else
      val scope = scopes.pop()
      scopes.push(scope + (name.lexeme -> true))