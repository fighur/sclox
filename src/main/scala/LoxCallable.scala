trait LoxCallable:
  def call(interpreter: Interpreter, arguments: List[Any]): Any
  def arity(): Int
