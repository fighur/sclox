
import scala.util.{Try, Success, Failure}

import TokenType.*

class Interpreter:
  private class ClockFun extends LoxCallable:
    override def arity(): Int = 0
    override def call(interpreter: Interpreter, arguments: List[Any]): Any =
      System.currentTimeMillis() / 1000.0
    override def toString(): String = "<native fn>"

  private val globals = Environment()
  globals.define("clock", ClockFun())

  private var locals = Map.empty[Expr, Int]
  private var environment = globals

  def resolve(expr: Expr, depth: Int): Unit =
    locals += (expr -> depth)


  def interpret(statements: List[Stmt]): Unit =
    try
      for statement <- statements do execute(statement)
    catch
      case re: RuntimeError => Lox.runtimeError(re)


  private def execute(stmt: Stmt): Unit = stmt match
    case Stmt.Expression(expr) => evaluate(expr)
    case stmt @ Stmt.Function(name, _, _) =>
      val function = LoxFunction(stmt, environment, false)
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

    case Stmt.Class(name, methods) =>
      environment.define(name.lexeme, null)

      val classMethods = methods.map(method =>
        val methodName = method.name.lexeme
        (methodName, LoxFunction(method, environment, methodName == "init"))).toMap
      val klass = LoxClass(name.lexeme, classMethods)

      environment.assign(name, klass)

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
        case MINUS => rightV match
          case d: Double => -d
          case _ => numberOperandError(operator)
        case BANG => !isTruthy(rightV)
        case _ => ???

    case Expr.Logical(left, operator, right) =>
      val leftV = evaluate(left)
      operator.tokenType match
        case OR if isTruthy(leftV) => leftV
        case AND if !isTruthy(leftV) => leftV
        case _ => evaluate(right)

    case Expr.Variable(name) => lookUpVariable(name, expr)
    case Expr.Assign(name, value) =>
      val evaluatedValue = evaluate(value)
      locals.get(expr) match
        case Some(distance) => environment.assignAt(distance, name, evaluatedValue)
        case None => globals.assign(name, evaluatedValue)
      evaluatedValue

    case Expr.Binary(left, operator, right) =>
      val leftV = evaluate(left)
      val rightV = evaluate(right)
      operator.tokenType match
        case BANG_EQUAL => !isEqual(leftV, rightV)
        case EQUAL_EQUAL => isEqual(leftV, rightV)
        case GREATER => (leftV, rightV) match
          case (ld: Double, rd: Double) => ld > rd
          case _ => numberOperandsError(operator)
        case GREATER_EQUAL => (leftV, rightV) match
          case (ld: Double, rd: Double) => ld >= rd
          case _ => numberOperandsError(operator)
        case LESS => (leftV, rightV) match
          case (ld: Double, rd: Double) => ld < rd
          case _ => numberOperandsError(operator)
        case LESS_EQUAL => (leftV, rightV) match
          case (ld: Double, rd: Double) => ld <= rd
          case _ => numberOperandsError(operator)
        case MINUS => (leftV, rightV) match
          case (ld: Double, rd: Double) => ld - rd
          case _ => numberOperandsError(operator)
        case PLUS => (leftV, rightV) match
          case (ld: Double, rd: Double) => ld + rd
          case (ls: String, rs: String) => ls + rs
          case _ => throw RuntimeError(operator, "Operands must be two numbers or two strings.")
        case SLASH => (leftV, rightV) match
          case (ld: Double, rd: Double) => ld / rd
          case _ => numberOperandsError(operator)
        case STAR => (leftV, rightV) match
          case (ld: Double, rd: Double) => ld * rd
          case _ => numberOperandsError(operator)
        case _ => ???

    case Expr.Call(callee, paren, arguments) =>
      val calleeV = evaluate(callee)
      val argumentsV = arguments.map(evaluate(_))
      evaluate(callee) match
        case lc: LoxCallable =>
          if (arguments.length != lc.arity()) then
            throw RuntimeError(paren, s"Expected ${lc.arity()} arguments but got ${arguments.length}.")
          else
            lc.call(this, argumentsV)
        case _ =>
          throw RuntimeError(paren, "Can only call functions and classes.")

    case Expr.Get(instance, name) =>
      evaluate(instance) match
        case li: LoxInstance =>
          li.get(name)
        case _ => throw RuntimeError(name, "Only instances have properties.")

    case Expr.Set(instance, name, value) =>
      evaluate(instance) match
        case li: LoxInstance =>
          val valueV = evaluate(value)
          li.set(name, valueV)
          valueV
        case _ => throw RuntimeError(name, "Only instances have fields.")

    case Expr.This(keyword) => lookUpVariable(keyword, expr)

  end evaluate


  private def lookUpVariable(name: Token, expr: Expr): Any =
    locals.get(expr) match
      case Some(distance) => environment.getAt(distance, name.lexeme)
      case None => globals.get(name)


  private def isTruthy(value: Any): Boolean = value match
    case b: Boolean => b
    case null => false
    case _ => true


  private def isEqual(a: Any, b: Any): Boolean =
    if (a == null && b == null) true
    else if (a == null) false
    else a.equals(b)


  private def numberOperandError(operator: Token): Nothing =
    throw RuntimeError(operator, "Operand must be a number.")


  private def numberOperandsError(operator: Token): Nothing =
    throw RuntimeError(operator, "Operands must be numbers.")


  private def stringify(value: Any): String = value match
    case null => "nil"
    case double: Double =>
      val text = double.toString
      if text.endsWith(".0") then text.substring(0, text.length - 2)
      else text
    case other => other.toString

end Interpreter