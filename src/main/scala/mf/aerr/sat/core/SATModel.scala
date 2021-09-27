package mf.aerr.sat.core

import mf.aerr.sat.core.SATModel.{Assertions, Declarations}
import mf.aerr.sat.core.objecttype.{AssertObject, DeclaredObject}

import java.io.{File, FileWriter}
import scala.language.postfixOps

/** Clase para gestionar el modelo SAT */
case class SATModel(declarations: Declarations, assertions: Assertions) {

  private val checkSAT: String = "(check-sat)"
  private val getModel: String = "(get-model)"

  private def close: String = {
    val closeLines: Seq[String] = Seq(checkSAT, getModel)
    (declarations ++ assertions ++ closeLines).mkString("\n")
  }

  val script: String = close

  def write(path: String): Unit = {
    val newFile = new File(path)
    val writer = new FileWriter(newFile)
    writer.append(script)
    writer.close()
  }
}

object SATModel {
  type Assertions = Seq[AssertObject]
  type Declarations = Seq[DeclaredObject]
}
