import scala.util.{Try, Success, Failure}

class LoxFunction(declaration: Stmt.Function, closure: Environment) extends LoxCallable:
  override def call(interpreter: Interpreter, arguments: List[Any]): Any =
    val environment = Environment(closure)
    declaration.params.zip(arguments).foreach((param, arg) =>
      environment.define(param.lexeme, arg))

    Try(interpreter.executeBlock(declaration.body, environment)) match
      case Failure(Return(value)) => value
      case Success(_) => null
      case _ => ???

  override def arity(): Int = declaration.params.length

  override def toString(): String = s"<fn ${declaration.name.lexeme}>"