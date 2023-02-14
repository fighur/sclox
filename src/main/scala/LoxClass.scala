class LoxClass(
    val name: String,
    superclass: Option[LoxClass],
    methods: Map[String, LoxFunction]
) extends LoxCallable:

  def findMethod(name: String): Option[LoxFunction] =
    methods.get(name) match
      case Some(method) => Some(method)
      case None =>
        superclass match
          case Some(klass) => klass.findMethod(name)
          case None        => None

  override def call(interpreter: Interpreter, arguments: List[Any]): Any =
    val instance = LoxInstance(this)

    findMethod("init") match
      case Some(initializer) =>
        initializer.bind(instance).call(interpreter, arguments)
      case None => ()

    instance

  override def arity(): Int = findMethod("init") match
    case Some(initializer) => initializer.arity()
    case None              => 0

  override def toString(): String = name
