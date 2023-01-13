import scala.util.{Try, Success, Failure}

import Expr.*
import TokenType.*

class Interpreter:
  def interpret(expr: Expr): Unit =
    Try(evaluate(expr)) match
      case Success(value) =>
        println(stringify(value))
      case Failure(re: RuntimeError) =>
        Lox.runtimeError(re)
      case _ => ???


  private def evaluate(expr: Expr): Any = expr match
    case Literal(value) => value
    case Grouping(expr) => evaluate(expr)
    case Unary(operator, right) =>
      val rightV = evaluate(right)
      operator.tokenType match
        case MINUS =>
          checkNumberOperand(operator, rightV)
          -rightV.asInstanceOf[Double]
        case BANG => !isTruthy(rightV)
        case _ => ???

    case Binary(left, operator, right) =>
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