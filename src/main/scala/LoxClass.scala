class LoxClass(val name: String, methods: Map[String, LoxFunction]) extends LoxCallable:

  def findMethod(name: String): Option[LoxFunction] = methods.get(name)

  override def call(interpreter: Interpreter, arguments: List[Any]): Any =
    val instance = LoxInstance(this)

    findMethod("init") match
      case Some(initializer) =>
        initializer.bind(instance).call(interpreter, arguments)
      case None => ()

    instance

  override def arity(): Int = findMethod("init") match
    case Some(initializer) => initializer.arity()
    case None => 0


  override def toString(): String = name