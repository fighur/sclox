
import scala.util.{Try, Success, Failure}

import TokenType.*

class Interpreter:
  private class ClockFun extends LoxCallable:
    override def arity(): Int = 0
    override def call(interpreter: Interpreter, arguments: List[Any]): Any =
      System.currentTimeMillis() / 1000.0
    override def toString(): String = "<native fn>"

  val globals = Environment()
  globals.define("clock", ClockFun())

  private var environment = globals

  def interpret(statements: List[Stmt]): Unit =
    statements.foldLeft(false)((failed, stmt) =>
      if failed then failed
      else Try(execute(stmt)) match
        case Success(_) => false
        case Failure(re: RuntimeError) =>
          Lox.runtimeError(re)
          true
        case _ => true
    )


  private def execute(stmt: Stmt): Unit = stmt match
    case Stmt.Expression(expr) => evaluate(expr)
    case stmt @ Stmt.Function(name, _, _) =>
      val function = LoxFunction(stmt, environment)
      environment.define(name.lexeme, function)

    case Stmt.If(condition, thenBranch, elseBranch) =>
      if isTruthy(evaluate(condition)) then execute(thenBranch)
      else elseBranch match
        case Some(stmt) => execute(stmt)
        case None => ()

    case Stmt.Print(expr) =>
      val value = evaluate(expr)
      println(stringify(value))

    case Stmt.Return(keyword, value) => value match
      case Some(value) => throw Return(evaluate(value))
      case None => throw Return(null)

    case Stmt.While(condition, body) =>
      while isTruthy(evaluate(condition)) do
        execute(body)

    case Stmt.Var(name, initializer) =>
      val value = initializer match
        case Some(expr) => evaluate(expr)
        case None => null
      environment.define(name.lexeme, value)

    case Stmt.Block(statements) =>
      executeBlock(statements, Environment(environment))

  end execute


  def executeBlock(statements: List[Stmt], environment: Environment): Unit =
    val previous = this.environment
    try
      this.environment = environment
      for stmt <- statements do
        execute(stmt)
    finally
      this.environment = previous


  private def evaluate(expr: Expr): Any = expr match
    case Expr.Literal(value) => value
    case Expr.Grouping(expr) => evaluate(expr)
    case Expr.Unary(operator, right) =>
      val rightV = evaluate(right)
      operator.tokenType match
        case MINUS =>
          checkNumberOperand(operator, rightV)
          -rightV.asInstanceOf[Double]
        case BANG => !isTruthy(rightV)
        case _ => ???

    case Expr.Logical(left, operator, right) =>
      val leftV = evaluate(left)
      operator.tokenType match
        case OR if isTruthy(leftV) => leftV
        case AND if !isTruthy(leftV) => leftV
        case _ => evaluate(right)

    case Expr.Variable(name) => environment.get(name)
    case Expr.Assign(name, value) =>
      val evaluatedValue = evaluate(value)
      environment.assign(name, evaluatedValue)
      evaluatedValue

    case Expr.Binary(left, operator, right) =>
      val leftV = evaluate(left)
      val rightV = evaluate(right)
      operator.tokenType match
        case BANG_EQUAL => !isEqual(leftV, rightV)
        case EQUAL_EQUAL => isEqual(leftV, rightV)
        case GREATER =>
          checkNumberOperands(operator, leftV, rightV)
          leftV.asInstanceOf[Double] > rightV.asInstanceOf[Double]
        case GREATER_EQUAL =>
          checkNumberOperands(operator, leftV, rightV)
          leftV.asInstanceOf[Double] >= rightV.asInstanceOf[Double]
        case LESS =>
          checkNumberOperands(operator, leftV, rightV)
          leftV.asInstanceOf[Double] < rightV.asInstanceOf[Double]
        case LESS_EQUAL =>
          checkNumberOperands(operator, leftV, rightV)
          leftV.asInstanceOf[Double] <= rightV.asInstanceOf[Double]
        case MINUS =>
          checkNumberOperands(operator, leftV, rightV)
          leftV.asInstanceOf[Double] - rightV.asInstanceOf[Double]
        case PLUS =>
          if leftV.isInstanceOf[Double] && rightV.isInstanceOf[Double] then
            leftV.asInstanceOf[Double] + rightV.asInstanceOf[Double]
          else if leftV.isInstanceOf[String] && rightV.isInstanceOf[String] then
            leftV.asInstanceOf[String] + rightV.asInstanceOf[String]
          else
            throw RuntimeError(operator, "Operands must be two numbers or two strings.")
        case SLASH =>
          checkNumberOperands(operator, leftV, rightV)
          leftV.asInstanceOf[Double] / rightV.asInstanceOf[Double]
        case STAR =>
          checkNumberOperands(operator, leftV, rightV)
          leftV.asInstanceOf[Double] * rightV.asInstanceOf[Double]
        case _ => ???

    case Expr.Call(callee, paren, arguments) =>
      val calleeV = evaluate(callee)
      val argumentsV = arguments.map(evaluate(_))
      if !calleeV.isInstanceOf[LoxCallable] then
        throw RuntimeError(paren, "Can only call functions and classes.")
      else
        val function = calleeV.asInstanceOf[LoxCallable]
        if (arguments.length != function.arity()) then
          throw RuntimeError(paren, s"Expected ${function.arity()} arguments but got ${arguments.length}.")
        else
          function.call(this, argumentsV)

  end evaluate


  private def isTruthy(value: Any): Boolean =
    if (value == null) false
    else if (value.isInstanceOf[Boolean]) value.asInstanceOf[Boolean]
    else true


  private def isEqual(a: Any, b: Any): Boolean =
    if (a == null && b == null) true
    else if (a == null) false
    else a.equals(b)


  private def checkNumberOperand(operator: Token, operand: Any): Unit =
    if !operand.isInstanceOf[Double] then
      throw RuntimeError(operator, "Operand must be a number.")


  private def checkNumberOperands(operator: Token, left: Any, right: Any): Unit =
    if !left.isInstanceOf[Double] || !right.isInstanceOf[Double] then
      throw RuntimeError(operator, "Operands must be numbers.")


  private def stringify(value: Any): String = value match
    case null => "nil"
    case double: Double =>
      val text = double.toString
      if text.endsWith(".0") then text.substring(0, text.length - 2)
      else text
    case other => other.toString

end Interpreter