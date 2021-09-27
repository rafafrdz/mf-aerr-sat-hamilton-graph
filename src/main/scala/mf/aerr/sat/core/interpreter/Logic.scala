package mf.aerr.sat.core.interpreter

/** DSL de logica (muy basico) */

trait AbstractLogic[P, T] {
  def value(v: P): T

  def not(l: T): T

  def or(l1: T*): T

  def and(l1: T*): T
}

trait Then[T] {
  def ==>(l1: T, l2: => T): T
}

trait Logic[T] extends AbstractLogic[T, T]

trait ThenLogic[T] extends Logic[T] with Then[T]

trait ThenAbstractLogic[P, T] extends AbstractLogic[P, T] with Then[T]

object Logic {
  implicit val logicStr: ThenLogic[String] = new ThenLogic[String] {
    def value(v: String): String = v

    def not(l: String): String = s"(not $l)"

    def or(l: String*): String =
      if (l.length > 1) s"(or ${l.mkString(" ")})" else l.head

    def and(l: String*): String =
      if (l.length > 1) s"(and ${l.mkString(" ")})" else l.head

    def ==>(l1: String, l2: => String): String = s"(=> $l1 $l2)"
  }

  implicit val logicBool: ThenLogic[Boolean] = new ThenLogic[Boolean] {
    override def value(v: Boolean): Boolean = v

    override def not(l: Boolean): Boolean = !l

    override def or(l: Boolean*): Boolean = l.reduceLeft(_ || _)

    override def and(l: Boolean*): Boolean = l.reduceLeft(_ && _)

    override def ==>(l1: Boolean, l2: => Boolean): Boolean = !l1 || l2
  }

  implicit val logicBoolString: ThenAbstractLogic[Boolean, String] = new ThenAbstractLogic[Boolean, String] {
    private val logic: ThenLogic[String] = implicitly[ThenLogic[String]]

    override def value(v: Boolean): String = v.toString

    override def not(l: String): String = logic.not(l)

    override def or(l: String*): String = logic.or(l: _*)

    override def and(l: String*): String = logic.and(l: _*)

    override def ==>(l1: String, l2: => String): String = logic.==>(l1, l2)
  }

  def useLogic[T](implicit logic: ThenLogic[T]): ThenLogic[T] = logic

  def useAbsLogic[P, T](implicit logic: AbstractLogic[P, T]): AbstractLogic[P, T] = logic
}
