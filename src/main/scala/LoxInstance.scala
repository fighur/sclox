class LoxInstance(klass: LoxClass):
  private var fields = Map.empty[String, Any]

  def get(name: Token): Any =
    fields.get(name.lexeme) match
      case Some(field) => field
      case None =>
        klass.findMethod(name.lexeme) match
          case Some(method) => method.bind(this)
          case None =>
            throw RuntimeError(name, s"Undefined property '${name.lexeme}'.")

  def set(name: Token, value: Any): Unit =
    fields += (name.lexeme -> value)

  override def toString(): String = s"${klass.name} instance"
