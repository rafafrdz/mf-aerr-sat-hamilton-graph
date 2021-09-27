package mf.aerr.sat.core.objecttype

import mf.aerr.sat.core.datatype.DataType

/** AST para gestionar las declaraciones del modelo SAT */

sealed trait ObjectType

trait DeclaredObject extends ObjectType

trait AssertObject extends ObjectType

case class Const(name: String, constType: DataType) extends DeclaredObject {
  override def toString: String = s"(declare-const $name ${constType.expr})"
}

case class Assert[T](cond: T) extends AssertObject {
  override def toString: String = s"(assert $cond)"
}
