class Environment(enclosing: Environment = null):
  private var values = Map.empty[String, Any]

  def define(name: String, value: Any): Unit =
    values += (name -> value)


  def get(name: Token): Any =
    if values.contains(name.lexeme) then
      values(name.lexeme)
    else if enclosing != null then
      enclosing.get(name)
    else
      throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")


  def assign(name: Token, value: Any): Unit =
    if values.contains(name.lexeme) then
      values += (name.lexeme -> value)
    else if enclosing != null then
      enclosing.assign(name, value)
    else
      throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
