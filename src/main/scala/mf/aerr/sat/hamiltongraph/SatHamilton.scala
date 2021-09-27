package mf.aerr.sat.hamiltongraph

import mf.aerr.sat.core.SATModel
import mf.aerr.sat.core.SATModel.{Assertions, Declarations}
import mf.aerr.sat.core.datatype.BooleanType
import mf.aerr.sat.core.interpreter.Logic.useLogic
import mf.aerr.sat.core.interpreter.ThenLogic
import mf.aerr.sat.core.objecttype.{Assert, Const}
import mf.aerr.sat.hamiltongraph.HamiltonGraph.{hamiltonGraph, noHamiltonGraph}
import mf.aerr.sat.hamiltongraph.graph.NGraph

object SatHamilton {

  type Eval = String
  val logic: ThenLogic[Eval] = useLogic[Eval]

  import logic._

  def main(args: Array[String]): Unit = {
    val hamiltonModel: SATModel = hamiltonSATModel(hamiltonGraph)
    val noHamiltonModel: SATModel = hamiltonSATModel(noHamiltonGraph)

    /** Estas rutas son relativas al propio proyecto, pero se pueden definir otras
     * o bien parametrizarlo mediante los argumentos del main y hacer la llamada al propio jar compilado */
    hamiltonModel.write("src/main/resources/hamilton-sat-model.smt")
    noHamiltonModel.write("src/main/resources/no-hamilton-sat-model.smt")
  }


  /**
   * Metodo para obtener el script de modelo SAT
   *
   * @param graph objeto grafo que representa el grafo del cual se quiere obtener el modelo
   * @return modelo SAT del grafo
   */
  def hamiltonSATModel(graph: NGraph): SATModel = {
    val nodos: Seq[Int] = graph.index
    val position: Seq[Int] = 0 until graph.length
    val sigma: Int => Int => String = (i: Int) => (k: Int) => s"s$i$k"

    /**
     * Se definen todas las declaraciones para el modelo SAT
     * Se toman todos los indices de los nodos y se definen los sigma(i)(p)
     * donde i es el indice del nodo y p la posicion en la secuencia.
     *
     * En este caso, considerando el nodo n1 para la 5nta posicion de tipo BooleanType
     * return: (declare-const s14 Bool)
     */
    val dec: Declarations = nodos.flatMap(i => position.map(sigma(i))).map(name => Const(name, BooleanType))

    /**
     * Primera restriccion.
     *
     * For each i ∈ {1..n}, vi appears in the sequence
     * i.e. si0 V si1 V ... V si(n-1)
     */
    val cOne: Assertions =
      nodos.map(i => position.map(sigma(i)))
        .map(ri => or(ri: _*))
        .map(Assert[Eval])

    /**
     * Segunda restriccion.
     * For each i ∈ {1..n}, vi does not appear in two different positions of the sequence
     * i.e. sik => ~si0 /\ ~si1 /\ ... /\ ~si(k-1) /\ ~si(k+1) /\ ... /\ ~si(n-1)
     */
    val rTwo: Int => Assertions =
      (i: Int) => position.map(k => (i, k, position.withFilter(l => k != l).map(l => not(sigma(i)(l)))))
        .map { case (i, k, ri) => ==>(sigma(i)(k), and(ri: _*)) }
        .map(Assert[Eval])

    val cTwo: Assertions = nodos.flatMap(rTwo)


    /**
     * Tercera restriccion.
     * For each i ∈ {1..n}, the k-th element of the sequence contains a vertex
     * i.e. s1k V s2k V ... V snk
     *
     */
    val cThree: Assertions =
      position.map(k => nodos.map(i => sigma(i)(k)))
        .map(ri => or(ri: _*))
        .map(Assert[Eval])


    /**
     * Cuarta restriccion.
     * For each i , j ∈ {1..n} such that i =/= j , vi and vj do not appear in the same position of the sequence
     * i.e. sik => ~s1k /\ ~s2k /\ ... /\ ~s(i-1)k /\ ~s(i+1)k /\ ... /\ ~snk
     */
    val rFour: Int => Assertions =
      (k: Int) => nodos.map(i => (i, k, nodos.withFilter(j => i != j).map(j => not(sigma(j)(k)))))
        .map { case (i, k, ri) => ==>(sigma(i)(k), and(ri: _*)) }
        .map(Assert[Eval])

    val cFour: Assertions = position.flatMap(rFour)


    /**
     * Quinta restriccion.
     * For each i , j ∈ {1..n} such that i =/= j : if vi and vj are not adjacent in the graph,
     * then they do not appear together in the sequence.
     *
     */
    val rFive =
      (k: Int) => nodos.map(i => (i, k, graph.strangers(i).map(j => not(sigma(j)(k + 1)))))
        .map { case (i, k, ri) => ==>(sigma(i)(k), and(ri: _*)) }
        .map(Assert[Eval])

    val cFive = position.init.flatMap(rFive)


    /** Todas las declaraciones y restricciones */
    val allDeclarations: Declarations = dec
    val allAssertion: Assertions = cOne ++ cTwo ++ cThree ++ cFour ++ cFive
    SATModel(allDeclarations, allAssertion)
  }

}
