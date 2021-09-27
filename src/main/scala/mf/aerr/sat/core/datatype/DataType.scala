package mf.aerr.sat.core.datatype

/** Enum para los distintos tipos de declaracion en el modelo SAT
 * Ahora mismo solo BooleanType (para el caso de uso) */

sealed trait DataType {
  def expr: String
}

case object BooleanType extends DataType {
  def expr: String = "Bool"
}
