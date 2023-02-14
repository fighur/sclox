import scala.util.{Try, Success, Failure}

class LoxFunction(
    declaration: Stmt.Function,
    closure: Environment,
    isInitializer: Boolean
) extends LoxCallable:

  def bind(instance: LoxInstance): LoxFunction =
    val environment = Environment(closure)
    environment.define("this", instance)
    LoxFunction(declaration, environment, isInitializer)

  override def call(interpreter: Interpreter, arguments: List[Any]): Any =
    val environment = Environment(closure)
    declaration.params
      .zip(arguments)
      .foreach((param, arg) => environment.define(param.lexeme, arg))

    Try(interpreter.executeBlock(declaration.body, environment)) match
      case Failure(Return(_)) if isInitializer => closure.getAt(0, "this")
      case Failure(Return(value))              => value
      case Success(_) if isInitializer         => closure.getAt(0, "this")
      case Success(_)                          => null
      case Failure(error)                      => throw error

  override def arity(): Int = declaration.params.length
  override def toString(): String = s"<fn ${declaration.name.lexeme}>"
