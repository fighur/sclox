
import scala.util.{Try, Success, Failure}

import TokenType.*

class Interpreter:
  private var environment = Environment()

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
    case Stmt.Print(expr) =>
      val value = evaluate(expr)
      println(stringify(value))
    case Stmt.Var(name, initializer) =>
      val value = if initializer != null then evaluate(initializer) else null
      environment.define(name.lexeme, value)
    case Stmt.Block(statements) =>
      executeBlock(statements, Environment(environment))


  private def executeBlock(statements: List[Stmt], environment: Environment): Unit =
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